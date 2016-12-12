module Common
(
  Result(..),
  bytesToInteger,
  integerToBytes,
  safeIdx,
  findM,
  mayapply,
  splitOn,
  showHex,
  hPutBytes,
  toM
)
where

import Data.Bits
import Data.Word
import Data.Char
import Data.Maybe
import Data.List

import Data.Foldable
import Control.Applicative
import Control.Monad

import Foreign.Marshal.Array

import System.IO

type Result a = Either String a

-- | Packs a Big Endian @['Word8']@ into an 'Integer'. Since the
-- 'Integer' type is implemented using GMP (or an unboxed Int#,
-- size depending), it can be arbitrarily large and hold a
-- theoretically infinite # of bytes.
bytesToInteger :: [Word8] -> Integer
bytesToInteger = f 0 0
  where f acc _ [] = acc
        f acc i (b:bs) = f (acc + ((fromIntegral b) * (256 ^ i))) (i + 1) bs

-- | Dual to 'bytesToInteger'.
integerToBytes :: Integer -> [Word8]
integerToBytes = unfoldr popByte
  where
    popByte :: Integer -> Maybe (Word8, Integer)
    popByte 0 = Nothing
    popByte i = Just ((fromIntegral a), b)
      where (b, a) = quotRem i 256

-- | Safely gets the @i@th element in the list @xs@.
safeIdx :: Int -> [a] -> Maybe a
safeIdx i xs
  | length xs > i = Just $ xs !! i
  | otherwise = Nothing

toM :: Either l r -> Maybe r
toM (Left _) = Nothing
toM (Right r) = Just r

-- | Monadic version of 'find'.
findM :: (Monad m, Foldable t) => (a -> m Bool) -> t a -> m (Maybe a)
findM f = foldlM findf Nothing
  where findf Nothing a = do
          b <- f a
          return $ if b then Just a else Nothing
        findf (Just a) _ = return $ Just a

-- | Apply a binary monadic function over two 'Maybe' values,
-- returning a monadic maybe value.
mayapply :: (Monad m) => (a -> b -> m c) -> Maybe a -> Maybe b -> m (Maybe c)
mayapply f (Just a) (Just b) = Just <$> f a b
mayapply _ _        _        = return Nothing

-- Splits a string on any of the following chars,
-- respecting single quotes.
splitOn :: [Char] -> String -> [String]
splitOn cs = unfoldr f
  where
    f [] = Nothing
    f ('\'':xs) = Just (('\'':x') ++ take 1 xs' , drop 1 xs')
      where (x', xs') = span (/= '\'') xs
    f xs = Just (v, dropWhile isSplitter vs)
      where (v, vs) = span (not . isSplitter) xs
    isSplitter c = c `elem` cs

showHex :: [Word8] -> String
showHex = mconcat . map toPair
  where
    toPair w = [f h, f l] 
      where
        f x
          | x < 10 = chr $ ord '0' + (fromIntegral x)
          | otherwise = chr $ ord 'A' + (fromIntegral $ x - 10)
        l = w .&. 0x0F
        h = shiftR (w .&. 0xF0) 4
  

hPutBytes :: Handle -> [Word8] -> IO ()
hPutBytes hdl bytes = withArray bytes (flip (hPutBuf hdl) (length bytes))

