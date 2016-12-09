{-|
Module      : Buffer
Description : Easy binary operations with bits and bytes
Copyright   : (c) Nathaniel Symer, 2016
License     : MIT
Maintainer  : nate@symer.io
Stability   : stable
Portability : POSIX

A @Buffer@ is a managed C pointer with a size andoffset associated with it.
This data structure stores the first bit in the lowest index and the last bit in the hightest
index (big endian). This conflicts with our little-endian architecture.

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
  Buffer,
  newBuffer,
  freeBuffer,
  fromBits,
  length,
  isAlive,
  setBit,
  getBit,
  bufcpy,
  plusBuf,
  hPutBuffer
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
import System.IO
import System.IO.Unsafe
import Data.Maybe
import Data.Monoid
import Data.Char
import Data.Bool

idx2bitbyte :: Int -> (Int, Int)
idx2bitbyte idx = (7 - mod idx 8, div idx 8)

bitbyte2idx :: (Int, Int) -> Int
bitbyte2idx (bit, byte) = (7 - bit) + (byte * 8)

bits2bytes :: Int -> Int
bits2bytes b = d + (if m == 0 then 0 else 1)
  where (d,m) = divMod b 8

toBits :: FiniteBits a => a -> [Bool]
toBits x = map (testBit x . addTup . idx2bitbyte) [0..bitlen - 1]
  where addTup (a, b) = a + b
        bitlen = finiteBitSize x

foreign import ccall unsafe "string.h" memcpy :: Ptr a -> Ptr a -> CSize -> IO (Ptr a)
foreign import ccall unsafe "stdlib.h" malloc :: CSize -> IO (Ptr a)
foreign import ccall unsafe "stdlib.h" calloc :: CSize -> CSize -> IO (Ptr a)
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

hPutBuffer :: Handle -> Buffer -> IO ()
hPutBuffer hdl buf = void $ withPtr buf $ \ptr -> hPutBuf hdl ptr (bits2bytes $ length buf)

showBufferHex :: Buffer -> IO String
showBufferHex = f ""
  where
    f :: String -> Buffer -> IO String
    f acc buf
      | length buf == 0 = return acc
      | otherwise = do
        nybble <- packNybble
                    <$> (fromMaybe False <$> getBit buf 0)
                    <*> (fromMaybe False <$> getBit buf 1)
                    <*> (fromMaybe False <$> getBit buf 2)
                    <*> (fromMaybe False <$> getBit buf 3)
        f (acc <> [(toHex $ fromIntegral nybble)]) (buf `plusBuf` 4) 
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
  ptr <- calloc (CSize $ fromIntegral $ bits2bytes len) (CSize 1)
  Buffer 0 len <$> newManagedPtr ptr

-- | Frees a buffer
freeBuffer :: Buffer -> IO ()
freeBuffer b@(Buffer _ _ ptr) = releasePtr ptr >> return ()

-- | Create a new buffer from a list of 'Bool's.
-- (if bits = x:xs, xs contains bits more significant than x).
fromBits :: [Bool] -> IO Buffer
fromBits bits = g 0 bits =<< newBuffer (P.length bits)
  where
    g _ [] buf = return buf
    g i (x:xs) buf = do
      setBit buf i x
      g (i + 1) xs buf

fromBitlike :: FiniteBits a => a -> IO Buffer
fromBitlike = fromBits . toBits

-- | Get the length of a buffer in bits.
length :: Buffer -> Int
length (Buffer offset len _) = len - offset

-- | Returns whether of not a buffer is alive.
isAlive :: Buffer -> IO Bool
isAlive (Buffer _ _ ptr) = isPtrAlive ptr

-- TODO: Fix everything for (msb) 76543210 | 76543210 (lsb)
-- (setBit and getBit were already fixed mostly)

-- | Memcpy in bits with buffers.
bufcpy :: Buffer -> Buffer -> Int -> IO Buffer
bufcpy db sb len = (withPtr db (\d -> (withPtr sb (\s -> f s d)))) >> return db
  where f src dest
          | destNLeadingBits /= srcNLeadingBits = do -- Copy up to 7 leading bits 
            orcpy (dec srcFirstByte) (dec destFirstByte) id (flip shift nLeadingBits) -- Copy up to 7 leading bits
            f (src `plusPtr` nLeadingBits) (dest `plusPtr` nLeadingBits)              -- and try again
          | otherwise = do
            memcpy destFirstByte srcFirstByte $ CSize $ fromIntegral cpyLenBytes      -- Copy as many full bytes as possible from the middle
            when (mod len 8 /= 0) $ orcpy destBytesEnd srcBytesEnd id id              -- Copy up to 7 trailing bits
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
        orcpy dptr sptr sf df = do
          (sbyte :: Word8) <- peek sptr
          (dbyte :: Word8) <- peek dptr
          poke dptr (sf sbyte .|. df dbyte)

-- | 'plusPtr' for 'Buffer's.
plusBuf :: Buffer -> Int -> Buffer
plusBuf (Buffer o l ptr) i = Buffer (min l (o + i)) l ptr

-- | Sets bit @i@ to @on@.
setBit :: Buffer -> Int -> Bool -> IO ()
setBit buf@(Buffer off _ _) i on = void $ withPtr buf f
  where (d,m) = divMod (off + i) 8
        -- mask = bool zeroBits (bit m) on
        maskf = (flip (bool Bits.clearBit Bits.setBit on)) (7 - m)
        f ptr = do
          (b :: Word8) <- peek ptr'
          poke ptr' $ maskf b
          where ptr' = ptr `plusPtr` d
   
-- | Gets bit @i@
getBit :: Buffer -> Int -> IO (Maybe Bool)
getBit buf@(Buffer off _ _) i = withPtr buf $ \ptr -> do
  (byte :: Word8) <- peek $ ptr `plusPtr` d
  return $ testBit byte (7 - m)
  where (d, m) = divMod (off + i) 8

-- | Sets BYTE @i@ to @b@
setByte :: Buffer -> Int -> Word8 -> IO ()
setByte buf@(Buffer off _ _) i byte
  | i + 1 > (bits2bytes $ length buf) = return ()
  | i < 0 = return ()
  | otherwise = void $ withPtr buf f
  where f ptr
          | m == 0 = poke aptr byte
          | otherwise  = do
            (a :: Word8) <- peek aptr
            (b :: Word8) <- peek bptr
            let a' = shiftL byte m .|. a
                b' = shiftR byte (8 - m) .|. b
            poke aptr a'
            poke bptr b'
          where (d, m) = divMod off 8
                aptr = ptr `plusPtr` (d + i)
                bptr = aptr `plusPtr` 1

-- | Gets BYTE @i@
getByte :: Buffer -> Int -> IO (Maybe Word8)
getByte buf@(Buffer off _ _) i
  | i >= bits2bytes (length buf) = return Nothing
  | i < 0 = return Nothing
  | otherwise = withPtr buf f
  where
    f ptr
      | m == 0 = peek firstByte
      | otherwise = do
        (a :: Word8) <- peek firstByte
        (b :: Word8) <- peek $ firstByte `plusPtr` 1
        return $ shiftR a m .|. shiftL b (8 - m)
      where (d, m) = divMod off 8
            firstByte = ptr `plusPtr` d

withPtr :: Buffer -> (Ptr Word8 -> IO a) -> IO (Maybe a)
withPtr (Buffer _ _ ptr) = withManagedPtr ptr

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

