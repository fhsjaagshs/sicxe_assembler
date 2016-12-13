module Common
(
  Result(..),
  bytesToInteger,
  integerToBytes,
  safeIdx,
  findM,
  bindResultM,
  applyResultA,
  applyResultA2,
  splitOn,
  showHex,
  hPutBytes,
  toM,
  fromM,
  fst',
  snd',
  thd',
  onJust
)
where

import Data.Bits
import Data.Word
import Data.Char
import Data.Maybe
import Data.Either
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

onJust :: Monad m => (a -> m b) -> Maybe a -> m (Maybe b)
onJust f = maybe (return Nothing) (fmap Just . f)

-- | Turns an 'Either l r' into a 'Maybe r'. Loses left value.
toM :: Either l r -> Maybe r
toM = either (const Nothing) Just

-- | Turns a 'Maybe r' into an either, providing @l@ on 'Nothing'.
fromM :: l -> Maybe r -> Either l r
fromM l = maybe (Left l) Right

-- | Monadic version of 'find'.
findM :: (Monad m, Foldable t) => (a -> m Bool) -> t a -> m (Maybe a)
findM f = foldlM findf Nothing
  where findf Nothing a = do
          b <- f a
          return $ if b then Just a else Nothing
        findf (Just a) _ = return $ Just a

-- | Apply a binary monadic function over two results.
applyResultA2 :: (Applicative m) => (a -> b -> m c) -> Result a -> Result b -> m (Result c)
applyResultA2 f (Right a)  (Right b) = Right <$> f a b
applyResultA2 _ (Left err) _         = pure $ Left err
applyResultA2 _ _          (Left err) = pure $ Left err

-- | Apply a unary monadic function over a result.
applyResultA :: (Applicative m) => (a -> m b) -> Result a -> m (Result b)
applyResultA f (Right v)  = Right <$> f v
applyResultA _ (Left err) = pure $ Left err

bindResultM :: (Monad m) => (a -> m b) -> m (Result a) -> m (Result b)
bindResultM f v = v >>= either (return . Left) (fmap Right . f)

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

fst' :: (a, b, c) -> a
fst' (a, _, _) = a

snd' :: (a, b, c) -> b
snd' (_, b, _) = b

thd' :: (a, b, c) -> c
thd' (_, _, c) = c
