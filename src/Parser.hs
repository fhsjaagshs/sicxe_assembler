{-|
Module: Parser
Description: Parse SIC/XE syntax using the Maybe monad.
License: Don't kill people, mmmmkay?
Maintainer: nate@symer.io

-}

{-# LANGUAGE OverloadedStrings #-}

module Parser
(
  Line(..),
  Operand(..),
  OperandType(..),
  Mnemonic(..),
  isComment,
  tokenizeLine,
  parseLine
)
where

import Common

import Data.Maybe
import Text.Read
import Data.List
import Data.Word
import Data.Bits
import Data.Char
import Control.Applicative

-- | Represents a line of SIC/XE assembly.
data Line = Line (Maybe String) Mnemonic [Operand] deriving (Eq, Show)

--
-- Tokenizer
--

-- | Determines if a line is a comment
isComment :: String -> Bool
isComment str = (safeIdx 0 $ dropWhile isWS str) == Just '.'
  where isWS ' '  = True
        isWS '\t' = True
        isWS _    = False

-- | Represents a token from @tokenizeLine@
type Token = String

-- | Splits line by whitespace, respecting single quotes
-- This will return an empty list of tokens on a comment line.
tokenizeLine :: String -> [Token]
tokenizeLine str = splitOn [' ', '\t'] str

--
-- Lines
--

-- | Parses a line of SIC/XE Assembly given a list of tokens.
parseLine :: [Token] -> Maybe Line
parseLine (l:n:o:_) = Line (nonnull l) <$> parseMnemonic n <*> parseOperands o
parseLine (l:n:_) = Line (nonnull l) <$> parseMnemonic n <*> pure []
parseLine _ = Nothing

--
-- Mnemonics
--

-- | Represents a mnemonic in SIC/XE.
data Mnemonic = Mnemonic String Bool deriving (Eq, Show)

-- | Parses a mnemonic from a token.
parseMnemonic :: Token -> Maybe Mnemonic
parseMnemonic = fmap f . nonnull 
  where f ('+':xs) = Mnemonic xs True
        f xs       = Mnemonic xs False

--
-- Operands
--

-- | The type of operand.
data OperandType = OpSimple | OpIndirect | OpImmediate deriving (Eq, Show)

-- | An operand to a SIC/XE instruction.
data Operand = Operand (Either Integer String) OperandType deriving (Eq, Show)

-- | Parse multiple operands in one token.
parseOperands :: Token -> Maybe [Operand]
parseOperands = mapM parseOperand . splitOn [',']

-- | Parse a single operand.
parseOperand :: Token -> Maybe Operand
parseOperand ('#':xs)      = (flip Operand OpImmediate) <$> (eitherOr numeric alphaNumeric xs)
parseOperand ('@':xs)      = (flip Operand OpIndirect) <$> (eitherOr numeric alphaNumeric xs)
parseOperand ('C':'\'':xs) = flip Operand OpImmediate . Left . bytesToInteger . map (fromIntegral . ord) <$> quoted ('\'':xs)
parseOperand ('X':'\'':xs) = flip Operand OpImmediate . Left . bytesToInteger <$> (quoted ('\'':xs) >>= hexstring)
parseOperand xs            = (flip Operand OpSimple) <$> (eitherOr numeric alphaNumeric xs)

--
-- Predicates
--
 
isNumeric :: Char -> Bool
isNumeric c = ord c >= ord '0' && ord c <= ord '9'

isAlphabetic :: Char -> Bool
isAlphabetic c = c' >= ord 'A' && c' <= ord 'Z'
  where c' = ord $ toUpper c

--
-- General parsers
--

-- | Parses a string in quotes: i.e.
-- the first char is a '\'', as is the last.
-- Excludes the quotes in the result.
quoted :: Token -> Maybe String
quoted = (=<<) f . nonnull
  where f ('\'':xs)
          | after == "'" = Just before
          | otherwise = Nothing 
          where (before,after) = span ((/=) '\'') xs

-- | Parses a string that has length > 0.
nonnull :: Token -> Maybe String
nonnull "" = Nothing
nonnull str = Just str

-- | Parses numeric strings.
numeric :: Token -> Maybe Integer
numeric = (=<<) readMaybe . predicated isNumeric

-- | Parses alphanumeric strings.
alphaNumeric :: Token -> Maybe String
alphaNumeric = predicated isAlphaNum

-- | Makes a parser from a predicate such that:
-- 1. For all characters c in the result, p(c(sub)i) = True
-- 2. The length of the result must equal the length of the input token.
predicated :: (Char -> Bool) -> Token -> Maybe String
predicated p str = nonnull =<< sequence (unfoldr (mkf p) str)
  where mkf _ [] = Nothing
        mkf pred (x:xs)
          | pred x = Just (Just x, xs) 
          | otherwise = Just (Nothing, "")

-- | Makes a parser that will succeed if one of @l@ and @r@ succeeds.
eitherOr :: (Token -> Maybe l) -> (Token -> Maybe r) -> Token -> Maybe (Either l r)
eitherOr l r str = (Left <$> l str) <|> (Right <$> r str)

--
-- Specific parsers
--

-- TODO: nonnull????
-- | Reads a binary value in hex from a 'String'.
hexstring :: String -> Maybe [Word8]
hexstring = fmap (group []) . mapM (hexchar . toUpper)
  where group :: [Word8] -> [Word8] -> [Word8]
        group acc [] = acc
        group acc (x:[]) = acc ++ [x]
        group acc (a:b:xs) = group (acc ++ [b + (a `shiftL` 4)]) xs

-- | Reads a hex nybble into the low bits of a byte
-- from a 'Char'.
hexchar :: Char -> Maybe Word8
hexchar a
  | between '0' '9' a = Just $ fromIntegral $ ord a - ord '0'
  | between 'A' 'F' a = Just $ fromIntegral $ (ord a - ord 'A') + 10
  | otherwise = Nothing
  where between a b c = ord b >= ord c && ord c >= ord a

