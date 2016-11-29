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

This module is dedicated to Fritz Wiedmer, my grandfather (~1925 to 2016). During
his time at IBM, he designed Bubbles memory and ECC for keyboards. Fritz is the reason
I became fascinated with computer science.

ENDIANNESS-independent
-}

{-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables #-}

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

-- IS SIC/XE big or litle endian?
-- Should this data structure take this into account?

import GHC.Prim
import Data.Bits hiding (setBit,clearBit)
import qualified Data.Bits as Bits (setBit,clearBit)
import Data.Word
import Control.Monad
import Foreign.Storable
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.ForeignPtr.Unsafe
import Foreign.Marshal.Alloc
import Foreign.C.Types
import Control.Concurrent.MVar
import Control.Exception (bracket)
import qualified Prelude as P (length)
import Prelude hiding (length)
import System.IO.Unsafe

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

instance Eq Buffer where
  NullBuffer == NullBuffer = True
  (Buffer aoff alen amptr) == (Buffer boff blen bmptr) = (aptr == bptr) && alen' == blen'
    where aptr = (unsafeManagedPtrToPtr amptr) `plusPtr` aoff
          alen' = alen - aoff
          bptr = (unsafeManagedPtrToPtr bmptr) `plusPtr` boff
          blen' = blen - boff
  _ == _ = False

-- TODO: Show instance that prints hexadecimal

-- TODO: parse string literals that contain binary escape sequences.

-- | Creates a null buffer
nullBuffer :: Buffer
nullBuffer = NullBuffer

-- | Creates a new buffer @len@ bits long.
newBuffer :: Int -> IO Buffer
newBuffer len = Buffer 0 len <$> (mallocBytes (bits2bytes len) >>= newManagedPtr)

-- | Frees a buffer
freeBuffer :: Buffer -> IO ()
freeBuffer b@(Buffer ptr rc) = 

-- | Create a new buffer from a list of 'Bool's.
fromBits :: [Bool] -> IO Buffer
fromBits bits = ((flip (foldM f)) bits) =<< newBuffer (P.length bits)
  where f buf bit = (plusBuf buf 1) <$ setBit buf 0 bit

-- | Get the length of a buffer in bits.
length :: Buffer -> Int
length (Buffer offset len _) = len - offset
length NullBuffer            = 0

-- | Returns whether or not a buffer is null.
isNull :: Buffer -> Bool
isNull NullBuffer = True
isNull _          = False

-- | Returns whether of not a buffer is alive.
isAlive :: Buffer -> IO Bool
isAlive NullBuffer = return False
isAlive (Buffer _ _ ptr) = isPtrAlive ptr

-- | Exactly like memcpy except for buffers & len is in bits.
bufcpy :: Buffer -> Buffer -> Int -> IO Buffer
bufcpy destb@(Buffer doff _ _) srcb@(Buffer soff _ _) len = destb <$ (withPtr destb (\d -> (withPtr srcb (\s -> f s d))))
  where f src dest = do
          -- Copy bits before the first byte
          when (dnbitsBeforeByte > 0) $ do
            (sbyte :: Word8) <- peek $ src `plusPtr` div soff 8
            (dbyte :: Word8) <- peek $ dest `plusPtr`  div doff 8
            let dbyte' = shift dbyte (snbitsBeforeByte - dnbitsBeforeByte)
            poke (dest `plusPtr` div doff 8) (sbyte .|. dbyte')
     
          -- Copy bits after the last byte
          when (dnbitsAfterByte > 0) $ do
            (sbyte :: Word8) <- peek $ src `plusPtr` (bits2bytes $ soff + len)
            (dbyte :: Word8) <- peek $ dest `plusPtr` (bits2bytes $ doff + len)
            poke (dest `plusPtr` (div (doff + len) 8)) (sbyte .|. dbyte)

          -- Copy the bytes in the middle
          memcpy (dest `plusPtr` dfirstByteIdx)  (src `plusPtr` sfirstByteIdx) cnumWholeBytesToCopy
        dnbitsBeforeByte = 8 - (mod doff 8) -- (DESTINATION) Number of bits before an even byte boundary (offset 15 -> dnbitsBeforeByte = 1)
        snbitsBeforeByte = 8 - (mod soff 8) -- (SOURCE)
        dnbitsAfterByte = 8 - (mod (doff + len) 8) -- (DESTINATION) number of bits after the last remaining byte
        snbitsAfterByte = 8 - (mod (soff + len) 8) -- (SOURCE)

        dfirstByteIdx = div (doff + dnbitsBeforeByte) 8
        sfirstByteIdx = div (soff + snbitsBeforeByte) 8
        numWholeBytesToCopy = div (len - dnbitsBeforeByte - dnbitsAfterByte) 8
        cnumWholeBytesToCopy = CSize $ fromIntegral numWholeBytesToCopy

-- | Copies @a@ at offset (from left) @aoff@ to @b@ at offset (from left) at @boff@.
-- bitcpy :: Word8 -> Word8 -> Word8 -> Word8 -> Word8
-- bitcpy _ b _    _    0 = b
-- bitcpy a b aoff boff i = bitcpy a b' (aoff + 1) (boff + 1) (i - 1)
--   where b' = setBit b (8 - boff) $ testBit (8 - aoff) a

-- TODO: Add values to NullBuffers treating them as 0.
-- | 'plusPtr' for 'Buffer's
-- This is the reason why pointers are as complicated as they are.
-- Say you create a buffer, and pass the result of a plusBuf to another thread,
-- then exit. Now, your original buffer would deallocate the memory the plusBuf version requires.
plusBuf :: Buffer -> Int -> Buffer
plusBuf NullBuffer _ = NullBuffer
plusBuf (Buffer o l ptr) i = Buffer (o + i) l (unsafePerformIO $ retainPtr ptr)

-- | Sets bit @i@ to @on@.
setBit :: Buffer -> Int -> Bool -> IO ()
setBit buf i = (<$) () . withPtr buf . xform . scBit
  where
    xform f p = peek p >>= poke p . f
    f p = p `plusPtr` (div i 8)
    scBit True v = Bits.setBit v (8 - (mod i 8))
    scBit False v = Bits.clearBit v (8 - (mod i 8))
   
-- | Gets bit @i@.
getBit :: Buffer -> Int -> IO (Maybe Bool)
getBit buf i = withPtr buf $ \ptr -> do
  (byte :: Word8) <- peek $ ptr `plusPtr` (length buf `div` 8)
  return $ testBit byte (8 - (mod i 8))

withPtr :: Buffer -> (Ptr Word8 -> IO a) -> IO (Maybe a)
withPtr NullBuffer _ = error "null buffer dereference error"
withPtr (Buffer _ _ ptr) f = withManagedPtr ptr f

--
-- INTERNAL
--     

foreign import ccall "wrapper" mkFinalizerFunPtr :: (Ptr Word8 -> Ptr Word8 -> IO ()) -> IO (FunPtr (Ptr Word8 -> Ptr Word8 -> IO ()))

-- TODO: check: MVars synchronize values across copies

-- | Defines a memory-managed, threadsafe pointer.
data ManagedPtr a = MP (Ptr a) (MVar Word8)

unsafeManagedPtrToPtr :: ManagedPtr a -> Ptr a
unsafeManagedPtrToPtr (MP p _) = p

-- | Access the contents of a managed ptr.
withManagedPtr :: ManagedPtr a -> (Ptr a -> IO b) -> IO (Maybe b)
withManagedPtr m@(MP ptr rc) f = bracket aq rel g
  where
    aq = takeMVar rc <* retainPtr m
    rel rc' = putMVar rc rc' <* releasePtr m
    g 0 = return Nothing
    g _ = Just <$> f ptr

newManagedPtr :: Ptr a -> IO (ManagedPtr a)
newManagedPtr p = MP p <$> newMVar 1

retainPtr :: ManagedPtr a -> IO (ManagedPtr a)
retainPtr m@(MP ptr rc) = do
  v <- takeMVar rc
  when (v /= 0) $ putMVar rc (v + 1)
  return m

releasePtr :: ManagedPtr a -> IO (ManagedPtr a)
releasePtr m@(MP ptr rc) = do
  v <- takeMVar rc
  if (v > 0)
    then putMVar rc (v - 1)
    else free ptr >> putMVar rc 0
  return m

isPtrAlive :: ManagedPtr a -> IO Bool
isPtrAlive (MP _ rc) = do
  v <- takeMVar rc
  putMVar rc v
  return (v > 0)

