{-|
Module      : Buffer
Description : Easy binary operations with bits and bytes
Copyright   : (c) Nathaniel Symer, 2016
License     : MIT
Maintainer  : nate@symer.io
Stability   : stable
Portability : POSIX

A @Buffer@ is a managed C pointer with a size andoffset associated with it

This is essentially a lighter-weight Haskell ByteString. It takes into account
the available memory of SIG/XE machines when storing lengths and indeces.
Designed to have as little overhead as possible. Also, I cannot use bytestring
in the spirit of academic integrity.

This module is dedicated to Fritz Wiedmer, my grandfather (~1925 to 2016). During
his time at IBM, he designed Bubbles memory and ECC for keyboards. Fritz is the reason
I became fascinated with computer science.

-}

{-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables #-}

module Buffer
(
  bits2bytes,
  bytes2bits,
  Buffer,
  newBuffer,
  freeBuffer,
  fromBits,
  fromStorable,
  length,
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
import Foreign.C.Types
import Control.Concurrent.MVar
import Control.Exception (bracket)
import qualified Prelude as P (length)
import Prelude hiding (length)
import System.IO.Unsafe
import Data.Maybe
import Data.Monoid
import Data.Char
import Data.Bool

bits2bytes :: Int -> Int
bits2bytes b = d + (if m == 0 then 0 else 1)
  where (d,m) = divMod b 8

bytes2bits :: Int -> Int
bytes2bits = (*) 8

foreign import ccall unsafe "string.h" memcpy :: Ptr a -> Ptr a -> CSize -> IO (Ptr a)
foreign import ccall unsafe "stdlib.h" malloc :: CSize -> IO (Ptr a)
foreign import ccall unsafe "stdlib.h" free   :: Ptr a -> IO ()

-- | Buffer type.
data Buffer = Buffer
                !Int -- ^ Offset in bits
                !Int -- ^ Length in bits, ignoring offset
                (ManagedPtr Word8) -- ^ Actual chunk of memory. See below.

instance Eq Buffer where
  (Buffer aoff alen amptr) == (Buffer boff blen bmptr) = (aptr == bptr) && alen' == blen'
    where aptr = (unsafeManagedPtrToPtr amptr) `plusPtr` aoff
          alen' = alen - aoff
          bptr = (unsafeManagedPtrToPtr bmptr) `plusPtr` boff
          blen' = blen - boff

instance Show Buffer where
  show = unsafePerformIO . showBufferHex

-- TODO: parse string literals that contain binary escape sequences.

showBufferHex :: Buffer -> IO String
showBufferHex = f ""
  where
    f :: String -> Buffer -> IO String
    f acc buf
      | length buf == 0 = return acc
      | otherwise = do
        -- TODO: replace fromJust with fromMaybe False
        nybble <- packNybble
                    <$> (fromMaybe False <$> getBit buf 0)
                    <*> (fromMaybe False <$> getBit buf 1)
                    <*> (fromMaybe False <$> getBit buf 2)
                    <*> (fromMaybe False <$> getBit buf 3)
        f ((toHex $ fromIntegral nybble):acc) (buf `plusBuf` 4)
    toHex v
      | v < 10 = chr $ v + 48 -- numeric hex
      | otherwise = chr $ (v - 10) + 65 -- alpha hex

{-# INLINE packNybble #-}
packNybble :: Bool -> Bool -> Bool -> Bool -> Word8
packNybble  a b c d = z a 1 .|. z b 2 .|. z c 4 .|. z d 8
  where z False _ = 0
        z True  n = n

-- | Creates a new buffer @len@ bits long.
newBuffer :: Int -> IO Buffer
newBuffer len = do
  ptr <- malloc $ CSize $ fromIntegral $ bits2bytes len
  Buffer 0 len <$> newManagedPtr ptr

-- | Frees a buffer
freeBuffer :: Buffer -> IO ()
freeBuffer b@(Buffer _ _ ptr) = releasePtr ptr >> return ()

-- | Create a new buffer from a list of 'Bool's.
-- (if bits = x:xs, xs contains bits more significant than x).
fromBits :: [Bool] -> IO Buffer
fromBits bits = g bits =<< newBuffer (P.length bits)
  where
    g [] buf = return buf
    g (x:xs) buf = do
      setBit buf (P.length bits - (P.length xs) - 1) x
      g xs buf

fromStorable :: Storable a => a -> IO Buffer
fromStorable s = do
  buf <- newBuffer $ 8 * (sizeOf s)
  withPtr buf (\ptr -> poke (castPtr ptr) s)
  return buf

-- | Get the length of a buffer in bits.
length :: Buffer -> Int
length (Buffer offset len _) = len - offset

-- | Returns whether of not a buffer is alive.
isAlive :: Buffer -> IO Bool
isAlive (Buffer _ _ ptr) = isPtrAlive ptr

-- | Like Data.Bits.shift, but works on buffers
bufshift :: Buffer -> Int -> IO Buffer
bufshift b dist = return b -- TODO: implement

-- | Memcpy in bits with buffers.
bufcpy :: Buffer -> Buffer -> Int -> IO Buffer
bufcpy db sb len = (withPtr db (\d -> (withPtr sb (\s -> f s d)))) >> return db
  where f src dest
          | destNLeadingBits /= srcNLeadingBits = do -- Copy up to 7 leading bits 
            orcpy (dec srcFirstByte) (dec destFirstByte) (flip shift nLeadingBits) -- Copy up to 7 leading bits
            f (src `plusPtr` nLeadingBits) (dest `plusPtr` nLeadingBits)           -- and try again
          | otherwise = do
            memcpy destFirstByte srcFirstByte $ CSize $ fromIntegral cpyLenBytes   -- Copy as many full bytes as possible from the middle
            when (mod len 8 /= 0) $ orcpy destBytesEnd srcBytesEnd id              -- Copy up to 7 trailing bits
          where
            srcFirstByte = src `plusPtr` bits2bytes soff
            destFirstByte = dest `plusPtr` bits2bytes doff
            srcBytesEnd = srcFirstByte `plusPtr` cpyLenBytes
            destBytesEnd = destFirstByte `plusPtr` cpyLenBytes

        doff = off db
        soff = off sb
        destNLeadingBits = mod doff 8
        srcNLeadingBits = mod soff 8
        nLeadingBits = max destNLeadingBits srcNLeadingBits
        cpyLenBytes = div (len - destNLeadingBits) 8 

        off (Buffer o _ _) = o
        dec p = p `plusPtr` (-1)
        orcpy dptr sptr f = do
          (sbyte :: Word8) <- peek sptr
          (dbyte :: Word8) <- peek dptr
          poke dptr (sbyte .|. f dbyte)

-- | 'plusPtr' for 'Buffer's.
plusBuf :: Buffer -> Int -> Buffer
plusBuf (Buffer o l ptr) i = Buffer (min l (o + i)) l ptr

-- | Sets bit @i@ to @on@.
setBit :: Buffer -> Int -> Bool -> IO ()
setBit buf@(Buffer off _ _) i on = void $ withPtr buf f
  where (d,m) = divMod (off + i) 8
        -- mask = bool zeroBits (bit m) on
        maskf = (flip (bool Bits.clearBit Bits.setBit on)) m
        f ptr = do
          (b :: Word8) <- peek ptr'
          -- poke ptr' $ mask .|. b
          poke ptr' $ maskf b
          where ptr' = ptr `plusPtr` d
   
-- | Gets bit @i@, treating the entire buffer as a big-endian bit set (left to right)
getBit :: Buffer -> Int -> IO (Maybe Bool)
getBit buf@(Buffer off _ _) i = withPtr buf $ \ptr -> do
  (byte :: Word8) <- peek $ ptr `plusPtr` d
  return $ testBit byte m
  where (d, m) = divMod (off + i) 8

withPtr :: Buffer -> (Ptr Word8 -> IO a) -> IO (Maybe a)
withPtr (Buffer _ _ ptr) f = withManagedPtr ptr f

--
-- INTERNAL
--     

-- TODO: check: MVars synchronize values across copies

-- | Defines a memory-managed, threadsafe pointer.
data ManagedPtr a = MP (Ptr a) (MVar Word8)

unsafeManagedPtrToPtr :: ManagedPtr a -> Ptr a
unsafeManagedPtrToPtr (MP p _) = p

-- | Access the contents of a managed ptr.
withManagedPtr :: ManagedPtr a -> (Ptr a -> IO b) -> IO (Maybe b)
withManagedPtr (MP ptr rc) f = do
  rc' <- takeMVar rc
  if rc' == 0
    then do
      putMVar rc rc'
      return Nothing
    else do
      v <- f ptr
      putMVar rc rc'
      return $ Just v

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

