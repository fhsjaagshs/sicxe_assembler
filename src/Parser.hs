{-|


-}

{-# LANGUAGE OverloadedStrings #-}

module Parser
(
  Line(..),
  Directive(..),
  DataDecl(..),
  Instruction(..),
  Operand(..),
  Comment(..),
  Register(..),
  Identifier(..),
  Immediate(..),
  parseLine
)
where

import Data.Word
import Data.Bits
import Data.Char
import Control.Applicative

data Line = LDirective Directive | LDataDecl DataDecl | LComment Comment | LInstruction Instruction

parseLine :: String -> Maybe Line
parseLine str = (LDataDecl <$> parseDataDecl str)
              <|> (LDirective <$> parseDirective str)
              <|> (LComment <$> parseComment str)
              <|> (LInstruction <$> parseInstruction str) 

--
-- Directives
--

-- TODO: implement

data Directive = Directive

parseDirective :: String -> Maybe Directive
parseDirective = const Nothing

--
-- Data Declarations
--

data DataDecl = DataDecl {
  dataDeclName :: String,
  dataDeclSizeBytes :: Int,
  dataDeclValue :: Maybe Immediate
} deriving (Eq, Show)

parseDataDecl :: String -> Maybe DataDecl
parseDataDecl = const Nothing

--
-- Instructions
--

data Instruction = Instruction Label Mnemonic [Operand] (Maybe Comment) deriving (Eq, Show)

parseInstruction :: String -> Maybe Instruction
parseInstruction = const Nothing

--
-- Mnemonics
--

data Mnemonic = Mnemonic String

-- TODO: ensure @str@ is a valid mnemonic
parseMnemonic :: String -> Maybe Mnemonic
parseMnemonic = Just . Mnemonic

--
-- Labels
-- 

data Label = Label String

parseLabel :: String -> Maybe Label
parseLabel = Just . Label

--
-- Operands
--

data Operand = ImmOperand Immediate | IdOperand Identifier | RegOperand Register deriving (Eq, Show)

parseOperands :: String -> Maybe [Operand]
parseOperands str = Nothing -- TODO: Implement me (needs action on ,X)

-- * Parse things like ,X on operands
parseOperand :: String -> Maybe Operand
parseOperand str = (ImmOperand <$> parseImmediate str)
                 <|> (RegOperand <$> parseRegister str)
                 <|> (IdOperand <$> parseIdentifier str)

--
-- Comments
--

data Comment = Comment String

parseComment :: String -> Maybe Comment
parseComment  = Just . Comment

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
-- Identifiers
--

data Identifier = Identifier String deriving (Eq, Show)

parseIdentifier :: String -> Maybe Identifier
parseIdentifier = Just . Identifier

--
-- Immediates
--

data Immediate = INumber Integer | IString String | IHex [Word8] deriving (Eq, Show)

parseImmediate :: String -> Maybe Immediate
parseImmediate ('#':xs) = Just $ INumber $ read xs
parseImmediate xs = f =<< quoted xs
  where f ('C', xs) = Just $ IString xs
        f ('X', xs) = IHex <$> toHex xs
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
