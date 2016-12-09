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
tokenizeLine str = unfoldr uf str
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
      | isWhitespace x = ("", xs)
      | otherwise = let (tokpart, remainder) = span (\c -> not $ isWhitespace c || isQuote c) (x:xs)
                        (tokpart2, remainder') = getok remainder'
                    in (tokpart ++ tokpart2, remainder')
    isWhitespace ' ' = True
    isWhitespace '\t' = True
    isWhitespace _ = False
    isQuote '\'' = True
    isQuote _ = False 

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

-- TODO: Finish implementing operand parsing

data Operand = ImmOperand Immediate
             | IdOperand String Bool -- "\tSTART\tIDENTIFIER" or "MULR A,B"
             | ConstOperand Int -- myword\tWORD\t300
               deriving (Eq, Show)

-- TODO: "@" prefix and ",X" suffix

parseOperands :: String -> Maybe [Operand]
parseOperands
parseOperands str = Nothing -- TODO: Implement me (needs action on ,X)

-- * Parse things like ,X on operands
parseOperand :: String -> Maybe Operand
parseOperand str = (ImmOperand <$> parseImmediate str)
                 <|> (ConstOperand <$> maybeRead str)
                 <|> (IdOperand <$> nonnull str)

--
-- Immediates
--

data Immediate = IConstant Integer | IIdentifier String | IBytes [Word8] deriving (Eq, Show)

parseImmediate :: String -> Maybe Immediate
parseImmediate ('#':xs) = (IConstant <$> maybeRead xs) <|> (Just $ IIdentifier xs)
parseImmediate xs = f =<< quoted xs
  where f ('C', xs) = Just $ IBytes $ map (fromIntegral . ord) xs
        f ('X', xs) = IBytes <$> toHex xs
        f _         = Nothing

--
-- Helpers
--

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

 
isNumeric :: Char -> Bool
isNumeric c = ord c >= ord '0' && ord c < ord '9'

quoted :: String -> Maybe (Char, String)
quoted (x:'\'':xs)
  | last xs == '\'' = Just $ (x, init xs)
  | otherwise = Nothing
quoted _ = Nothing

nonnull :: String -> Maybe a
nonnull "" = Nothing
nonnull str = Just str

