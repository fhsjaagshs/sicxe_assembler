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
  Directive(..),
  DirecType(..),
  Instruction(..),
  Operand(..),
  Comment(..),
  Register(..),
  Identifier(..),
  Immediate(..),
  Label(..),
  tokenizeLine,
  parseLine,
  lineLabel
)
where

import Data.Word
import Data.Bits
import Data.Char
import Control.Applicative

data Line = LDirective Directive | LInstruction Instruction deriving (Eq, Show)

lineLabel :: Line -> Maybe Label
lineLabel (LDirective l _ _) = l
lineLabel (LInstruction l _ _) = l

tokenizeLine :: String -> [String]
tokenizeLine ('.':xs) = []
tokenizeLine str = f [] "" False str
  where
    f strs acc False ('\t':xs)
      | length acc > 0 = f (strs ++ [acc]) "" False xs
      | otherwise = f strs acc False xs
    f strs acc inquotes ('\'':xs) = f strs (acc ++ "'") (not inquotes) xs
    f strs acc inquotes (x:xs) = f strs (acc ++ [x]) inquotes xs

parseLine :: [String] -> Maybe Line
parseLine toks = (LDirective <$> parseDirective toks) <|> (LInstruction <$> parseInstruction toks)

--
-- Directives
--

data DirecType = START | END | RESB | RESW | BYTE | WORD deriving (Eq, Show)

parseDirecType :: String -> Maybe DirecType
parseDirecType "START" = Just START
parseDirecType "END" = Just END
parseDirecType "RESB" = Just RESB
parseDirecType "RESW" = Just RESW
parseDirecType "BYTE" = Just BYTE
parseDirecType "WORD" = Just WORD
parseDirecType _ = Nothing

data Directive = Directive (Maybe Label) DirecType (Either Immediate String) deriving (Eq, Show)

parseDirective :: [String] -> Maybe Directive
parseDirective (l:t:v:_) = Directive <$> parseLabel l <*> parseDirecType t <*> (parseImmediate v <|> pure v)
parseDirective _ = Nothing

--
-- Instructions
--

data Instruction = Instruction (Maybe Label) Mnemonic [Operand] deriving (Eq, Show)

parseInstruction :: [String] -> Maybe Instruction
parseInstruction (l:n:op:_)


--
-- Generic Field parsing
--

parseField :: String -> (String -> a) -> Maybe a
parseField _ "" = Nothing
parseField f str = Just $ f str

--
-- Mnemonics
--

data Mnemonic = Mnemonic String deriving (Eq, Show)

-- TODO: ensure @str@ is a valid mnemonic
parseMnemonic :: String -> Maybe Mnemonic
parseMnemonic = parseField Mnemonic

--
-- Labels
-- 

data Label = Label String deriving (Eq, Show)

parseLabel :: String -> Maybe Label
parseLabel = parseField Label

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

data Comment = Comment String deriving (Eq, Show)

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
