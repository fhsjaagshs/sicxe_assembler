module Common
(
  bytesToInteger,
  safeIdx,
  findM,
  mayapply,
  splitOn,
  showHex,
  hPutBytes
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

-- | Packs a Big Endian @['Word8']@ into an 'Integer'. Since the
-- 'Integer' type is implemented using GMP (or an unboxed Int#,
-- size depending), it can be arbitrarily large and hold a
-- theoretically infinite # of bytes.
bytesToInteger :: [Word8] -> Integer
bytesToInteger = f 0 0
  where f acc _ [] = acc
        f acc i (b:bs) = f (acc + (2 ^ (8 * i))) (i + i) bs -- By using arithmetic, we skirt endianness woes.

-- TODO: implement me?
-- integerToBytes :: Integer -> [Word8]

-- | Safely gets the @i@th element in the list @xs@.
safeIdx :: Int -> [a] -> Maybe a
safeIdx i xs
  | length xs > i = Just $ xs !! i
  | otherwise = Nothing

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
splitOn cs = unfoldr uf
  where
    -- function for use with unfoldr that unfolds a list of tuples from a string
    uf str
      | length str == 0 = Nothing
      | otherwise = Just $ getok str

    -- parses a single token, respecting single quotes
    getok [] = ("", "")
    getok (x:xs)
      | isQuote x = let (inquotes, xs') = span (not . isQuote) xs
                        (tokpart, remainder) = getok $ drop 1 xs'
                    in (inquotes ++ tokpart ++ "'", drop 1 remainder)
      | isSplitC x = ("", xs)
      | otherwise = let (tokpart, remainder) = span (\c -> not $ isSplitC c || isQuote c) (x:xs)
                        (tokpart2, remainder') = getok remainder
                    in (tokpart ++ tokpart2, remainder')
    isQuote '\'' = True
    isQuote _ = False 
    isSplitC c = c `elem` cs

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

