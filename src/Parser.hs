{-|
Module: Parser
Description: Parse SIC/XE syntax using the Maybe monad.
Copyright: I don't want to sue my professors for copyright infringement!
License: Don't kill people, mmmmkay?
Maintainer: nate@symer.io
Stability: honestly, not much
Portability: Who cares?

-}

{-# LANGUAGE OverloadedStrings #-}

module Parser
(
  Line(..),
  Operand(..),
  Register(..),
  Immediate(..),
  tokenizeLine,
  parseLine
)
where

import Text.Read
import Data.Word
import Data.Bits
import Data.Char
import Control.Applicative

data Line = Line {
  lineLabel :: (Maybe String),
  lineMnemonic :: Mnemonic,
  lineOperands :: [Operand]
} deriving (Eq, Show)

--
-- Tokenizer
--

-- Splits line by whitespace, respecting quotes
tokenizeLine :: String -> [String]
tokenizeLine ('.':_) = []
tokenizeLine str = splitOn [' ', '\t'] str

splitOn :: [Char] -> String -> [String]
splitOn cs = unfoldr uf
  where
    -- function for use with unfoldr that unfolds a list of tuples from a string
    uf str
      | length str == 0 = Nothing
      | otherwise = Just $ getok str

    -- parses a single token, respecting single quotes
    getok [] = []
    getok (x:xs)
      | isQuote x = let (inquotes, xs') = span (not . isQuote)
                        (tokpart, remainder) = getok xs'
                    in (inquotes ++ tokpart ++ '\'', drop 1 remainder)
      | isSplitC x = ("", xs)
      | otherwise = let (tokpart, remainder) = span (\c -> not $ isSplitC c || isQuote c) (x:xs)
                        (tokpart2, remainder') = getok remainder'
                    in (tokpart ++ tokpart2, remainder')
    isQuote '\'' = True
    isQuote _ = False 
    isSplitC c = c `elem` cs

--
-- Lines
--

parseLine :: [String] -> Maybe Line
parseLine (l:n:o:_) = Just $ Line (nonnull l) <$> (parseMnemonic n) <*> parseOperands o
parseLine (l:n:_) = Just $ Line (nonnull l) (parseMnemonic n) []
parseLine _ = Nothing

--
-- Mnemonics
--

data Mnemonic = Mnemonic {
  mnemonic :: String,
  mnemonicExtended :: Bool
} deriving (Eq, Show)

parseMnemonic :: String -> Maybe Mnemonic
parseMnemonic = fmap f . nonnull 
  where f ('+':xs) = Mnemonic xs True
        f xs       = Mnemonic xs False

--
-- Operands
--

data OperandType = OpSimple | OpIndirect | OpImmediate deriving (Eq, Show)
data Operand = Operand {
  operandValue :: (Either Integer String),
  operandType :: OperandType
} deriving (Eq, Show)

parseOperands :: String -> Maybe [Operand]
parseOperands = mapM parseOperand . splitOn [',']

-- TODO: rework parsing C'' and X''
--       they may overlap registers
parseOperand :: String -> Maybe Operand
parseOperand ('#':xs) = (flip Operand OpImmediate) <$> (eitherOr numeric alphaNumeric)
parseOperand ('@':xs) = (flip Operand OpIndirect) <$> (eitherOr numeric alphaNumeric)
parseOperand ('C':x:xs) = flip Operand OpImmediate . bytesToInteger . map (fromIntegral . ord) <$> quoted (x:xs)
parseOperand ('X':x:xs) = flip Operand OpImmediate . bytesToInteger . toHex <$> quoted (x:xs)
parseOperand xs       = (flip Operand OpSimple) <$> (eitherOr numeric alphaNumeric) OpSimple

--
-- Hex & Byte Helpers
--

bytesToInteger :: [Word8] -> Integer
bytesToInteger = f [] 0
  where f acc _ [] = acc
        f acc i (b:bs) = f (acc + (2 ^ (8 * i))) bs

toHex :: String -> Maybe [Word8]
toHex = fmap (group []) . mapM (hexchar . toUpper)
  where group :: [Word8] -> [Word8] -> [Word8]
        group acc [] = acc
        group acc (x:[]) = acc ++ [x]
        group acc (a:b:xs) = group (acc ++ [b + (a `shiftL` 4)]) xs

hexchar :: Char -> Maybe Word8
hexchar a
  | between '0' '9' a = Just $ fromIntegral $ ord a - ord '0'
  | between 'A' 'F' a = Just $ fromIntegral $ (ord a - ord 'A') + 10
  | otherwise = Nothing
  where between a b c = ord b >= ord c && ord c >= ord a

--
-- Predicates
--
 
isNumeric :: Char -> Bool
isNumeric c = ord c >= ord '0' && ord c <= ord '9'

isAlphabetic :: Char -> Bool
isAlphabetic c = c' >= ord 'A' && c' <= ord 'Z'
  where c' = ord $ upcase c

isAlphaNum :: Char -> Bool
isAlphaNum c = isNumeric c || isAlphabetic c

--
-- General parsers
--

quoted :: String -> Maybe String
quoted = f =<< nonnull
  where f ('\'':xs)
          | null after = Just before
          | otherwise = Nothing 
        where (before,after) = span ((/=) '\'') xs

nonnull :: String -> Maybe a
nonnull "" = Nothing
nonnull str = Just str

numeric :: String -> Maybe a
numeric = predicated isNumeric

alphaNumeric :: String -> Maybe a
alphaNumeric = predicated isAlphaNum

-- | Makes a parser from a predicate
predicated :: (Char -> Bool) -> String -> Maybe String
predicated p str = nonnull =<< unfoldr (mkf p) str
  where mkf _ [] = Nothing
        mkf pred (x:xs)
          | pred x = Just (Just x, xs) 
          | otherwise = Just (Nothing, "")

-- | Makes a choice parser
eitherOr :: (String -> Maybe l) -> (String -> Maybe r) -> String -> Maybe (Either l r)
eitherOr l r str = (Left <$> l str) <|> (Right <$> r str)
