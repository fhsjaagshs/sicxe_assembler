{-|
Module      : Buffer
Description : Easy binary operations with bits and bytes
Copyright   : (c) Nathaniel Symer, 2016
License     : MIT
Maintainer  : nate@symer.io
Stability   : stable
Portability : POSIX

A @Buffer@ is a C pointer with a length, offset, and mutex associated with it.

This is essentially a lighter-weight Haskell ByteString. It takes into account
the available memory of SIG/XE machines when storing lengths and indeces.
Designed to have as little overhead as possible. Also, I cannot use bytestring
in the spirit of academic integrity.

-}

{-# LANGUAGE ForeignFunctionInterface #-}

module Buffer
(
  bits2bytes,
  bytes2bits,
  Buffer,
  nullBuffer,
  newBuffer,
  destroyBuffer,
  fromStorable,
  length,
  isNull,
  isAlive,
  setBit,
  getBit,
  bufcpy,
  plusBuf
)
where

import GHC.Prim
import Data.Bits
import Data.Word
import Foreign.Storable
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshall.Alloc
import Foreign.C.Types
import Control.Concurrent.MVar

bits2bytes :: Int -> Int
bits2bytes b = d + (if m == 0 then 0 else 1)
  where (d,m) = divMod b 8

bytes2bits :: Int -> Int
bytes2bits = (*) 8

foreign import ccall unsafe "string.h" memcpy :: Ptr a -> Ptr a -> CSize -> IO (Ptr a)

-- | Buffer type.
data Buffer = Buffer
                !Int -- ^ Offset in bits
                !Int -- ^ Length in bits, ignoring offset
                (ManagedPtr Word8) -- ^ Actual chunk of memory. See below.
            | NullBuffer

-- TODO Monoid instance
--        a. mconcat that does a single allocation
--        b. pure (no buffers are modified)

-- | Creates a null buffer
nullBuffer :: Buffer
nullBuffer = NullBuffer

-- | Creates a new buffer @len@ bits long.
newBuffer :: Int -> IO Buffer
newBuffer len = Buffer 0 len $ mallocBytes (bits2bytes len) >>= newManagedPtr

-- | Copies a storable into a buffer.
fromStorable :: Storable a => a -> IO Buffer
fromStorable s = do
  b <- newBuffer $ bytes2bits $ sizeOf s
  withPtr b $ \ptr -> poke ptr s
  return b

-- | Get the length of a buffer in bits.
length :: Buffer -> Int
length (Buffer offset len _) = len - offset

-- | Returns whether or not a buffer is null.
isNull :: Buffer -> Bool
isNull NullBuffer = True
isNull _          = False

-- | Returns whether of not a buffer is alive.
isAlive :: Buffer -> IO Bool
isAlive NullBuffer = False
isAlive (Buffer _ _ ptr) = isPtrAlive ptr

-- TODO: currently @len@ is in bytes, it should be in bits.
-- | Exactly like memcpy except for buffers.
bufcpy :: Buffer -> Buffer -> Int -> IO Buffer
bufcpy dest src len = withPtr dest (\d -> withPtr src $ \s -> memcpy d s (fromIntegral len)) >> return dest

-- | 'plusPtr' for 'Buffer's
-- This is the reason why pointers are as complicated as they are.
-- Say you create a buffer, and pass the result of a plusBuf to another thread,
-- then exit. Now, your original buffer would deallocate the memory the plusBuf version requires.
plusBuf :: Buffer -> Int -> Buffer
plusBuf NullBuffer _ = NullBuffer
plusBuf (Buffer o l ptr) i = Buffer (o + i) l (retainPtr ptr)

-- | Sets bit @i@ to @on@.
setBit :: Buffer -> Int -> Bool -> IO ()
setBit buf i on = withPtr buf $ xform $ scBit on
  where
    xform f p = peek p >>= poke p . f
    f p = p `plusPtr` (div i 8)
    scBit True v = setBit v (8 - (mod i 8))
    scBit False v = clearBit v (8 - (mod i 8))
   
-- | Gets bit @i@.
getBit :: Buffer -> Int -> IO Bool
getBit buf i = witPtr buf $ \ptr ->
  (flip testBit) (8 - (mod i 8)) <$> peek (plusPtr ptr $ div (length buf) 8)

--
-- INTERNAL
--

withPtr :: Buffer -> (Ptr Word8 -> IO a) -> IO a
withPtr NullBuffer = error "null buffer"
withPtr (Buffer _ _ ptr) f = withManagedPtr ptr f
      
foreign import ccall "wrapper" mkFinalizerFunPtr :: (Ptr Word8 -> Ptr Word8 -> IO ()) -> IO (FunPtr (Ptr Word8 -> Ptr Word8 -> IO ()))

-- | Defines a memory-managed, threadsafe pointer.
data ManagedPtr a = MP (Ptr Word8) (ForeignPtr a) (MVar Bool)

withManagedPtr :: ManagedPointer a -> (Ptr a -> IO b) -> IO b
withManagedPtr m@(MP rc fptr lock) f = do
  retainPtr m
  lock' <- takeMVar lock
  v <- withForeignPtr fptr f
  putMVar lock'
  releasePtr m
  return v

newManagedPtr :: Ptr a -> IO (ManagedPtr a)
newManagedPtr p = MP <$> mkfptr p <*> callocBytes 1 <*> newMVar True
  where
    mkfptr = newForeignPtrEnv (mkFinalizerFunPtr foreignPtrFinalizer)

retainPtr :: ManagedPtr a -> IO (ManagedPtr a)
retainPtr m@(MP rc fptr lock) = do
  touchForeignPtr fptr
  peek rc >>= poke rc . (+) 1
  return m

releasePtr :: ManagedPtr a -> IO (ManagedPtr a)
releasePtr m@(MP rc fptr lock) = do
  _ <- takeMVar lock
  touchForeignPtr fptr
  putMVar =<< decrement rc
  return m

isPtrAlive :: ManagedPtr a -> IO Bool
isPtrAlive (MP _ _ lock) = do
  v <- takeMVar lock
  putMVar lock v
  return v 

foreignPtrFinalizer :: Ptr Word8 -> Ptr Word8 -> IO ()
foreignPtrFinalizer rc ptr = do
  alive <- decrement rc
  when (not alive) $ do
    free rc
    free ptr

-- | Decrements the word at @ptr@, returning if it's still > 0.
decrement :: Ptr Word8 -> IO Bool
decrement ptr = do 
  v <- peek ptr
  when (v > 0) poke ptr (v - 1)
  return $ v > 0

