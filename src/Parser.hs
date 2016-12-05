{-|


-}

{-# LANGUAGE OverloadedStrings #-}

module Parser
(

)

import Data.Char
import Control.Applicative

--
-- Data Declarations
--

data DataDecl = DataDecl {
  dataDeclName :: String,
  dataDeclSizeBytes :: Int,
  dataDeclValue :: String -- TODO: data structure for immediates
} deriving (Eq, Show)

--
-- Operands
--

data Operand = ImmOperand Immediate | IdOperand Identifier | IdOperandRegister deriving (Eq, Show)

parseOperand :: String -> Maybe Operand
parseOperand str = parseImmediate str <|>> parseRegister str <|> parseIdentifier str

--
-- Registers
--

data Register :: A | X | L | PC | SW | B | T | F deriving (Eq, Show)

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
-- Identifiers
--

data Identifier = Identifier String deriving (Eq. Show)

parseIdentifier :: String -> Maybe Identifier
parseIdentifier = Just . Identifier

--
-- Immediates
--

data Immediate = INumber Int | IString String

parseImmediate :: String -> Maybe Immediate
parseImmediate ('#':xs) = Just $ INumber $ read xs
parseImmediate xs = f =<< quoted xs
  where f ('C', xs) = Just $ IString xs
        f ('X', xs) = INumber <$> toHex xs
        f _         = Nothing

--
-- Helpers
--

toHex :: String -> Maybe Int
toHex str = f 0 0 str
  where f i acc (x:xs)
	  | i == (length str) = acc
	  | otherwise = case hexchar x of
	    Just c' -> f (i + 1) (acc + (c' `shiftL` (i * 4)) xs
	    Nothing -> Nothing
	hexchar a
	  | ord a <= ord '9' && ord a >= ord '0' = Just $ ord a - ord '0'
	  | ord a <= ord 'F' && ord a >= ord 'A' = Just $ ord a - ord 'A'
	  | otherwise = Nothing

quoted :: String -> Maybe (Char, String)
quoted (x:'\'':xs)
  | last xs == '\'' = Just $ (x, init xs)
  | otherwise = Nothing
quoted _ _ = Nothing
