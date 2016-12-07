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
  lineMnemonic :: String,
  lineOperands :: [Operand]
} deriving (Eq, Show)

--
-- Tokenizer
--

-- Splits line by tab, respecting quotes
tokenizeLine :: String -> [String]
tokenizeLine ('.':xs) = []
tokenizeLine str = f [] "" False str
  where
    f strs acc False ('\t':xs)
      | length acc > 0 = f (strs ++ [acc]) "" False xs
      | otherwise = f strs acc False xs
    f strs acc inquotes ('\'':xs) = f strs (acc ++ "'") (not inquotes) xs
    f strs acc inquotes (x:xs) = f strs (acc ++ [x]) inquotes xs

--
-- Lines
--

parseLine :: [String] -> Maybe Line
parseLine (l:n:o:_) = Just $ Line (nonnull l) <$> nonnull n <*> parseOperands o
parseLine (l:n:_) = Just $ Line (nonnull l) (nonnull n) []
parseLine _ = Nothing

--
-- Operands
--

-- TODO: FINISH ME!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

data Operand = ImmOperand Immediate
             | RegOperand Register
             | IdOperand String     -- \tSTART\tIDENTIFIER
             | ConstOperand Int     -- myword\tWORD\t300
               deriving (Eq, Show)

parseOperands :: String -> Maybe [Operand]
parseOperands str = Nothing -- TODO: Implement me (needs action on ,X)

-- * Parse things like ,X on operands
parseOperand :: String -> Maybe Operand
parseOperand str = (ImmOperand <$> parseImmediate str)
                 <|> (RegOperand <$> parseRegister str)
                 <|> (ConstOperand <$> maybeRead str)
                 <|> (IdOperand <$> nonnull str)

--
-- Registers
--

data Register = A | X | L | PC | SW | B | T | F deriving (Eq, Show)

parseRegister :: String -> Maybe Register
parseRegister "A" = Just A
parseRegister "X" = Just X
parseRegister "L" = Just L
parseRegister "PC" = Just PC
parseRegister "SW" = Just SW
parseRegister "B" = Just B
parseRegister "T" = Just T
parseRegister "F" = Just F
parseRegister _ = Nothing

--
-- Immediates
--

data Immediate = INumber Integer | IBytes [Word8] deriving (Eq, Show)

parseImmediate :: String -> Maybe Immediate
parseImmediate ('#':xs) = Just $ INumber $ read xs
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
 
quoted :: String -> Maybe (Char, String)
quoted (x:'\'':xs)
  | last xs == '\'' = Just $ (x, init xs)
  | otherwise = Nothing
quoted _ = Nothing

nonnull :: String -> Maybe a
nonnull "" = Nothing
nonnull str = Just str

