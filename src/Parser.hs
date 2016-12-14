{-|
Module: Parser
Description: Parse SIC/XE syntax
-}

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

import Text.Read (readEither)
import Data.List
import Data.Word
import Data.Bits
import Data.Char

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
parseLine :: [Token] -> Result Line
parseLine (l:n:o:_) = Line (toM $ nonnull l) <$> parseMnemonic n <*> parseOperands o
parseLine (l:n:_) = Line (toM $ nonnull l) <$> parseMnemonic n <*> pure []
parseLine _ = Left "invalid number of whitespace-delineated columns"

--
-- Mnemonics
--

-- | Represents a mnemonic in SIC/XE.
data Mnemonic = Mnemonic String Bool deriving (Eq, Show)

-- | Parses a mnemonic from a token.
parseMnemonic :: Token -> Result Mnemonic
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
parseOperands :: Token -> Result [Operand]
parseOperands = mapM parseOperand . splitOn [',']

-- | Parse a single operand.
parseOperand :: Token -> Result Operand
parseOperand ('#':xs)      = (flip Operand OpImmediate) <$> (choice numeric alphaNumeric xs)
parseOperand ('@':xs)      = (flip Operand OpIndirect) <$> (choice numeric alphaNumeric xs)
parseOperand ('C':'\'':xs) = flip Operand OpImmediate . Left . bytesToInteger . map (fromIntegral . ord) <$> quoted ('\'':xs)
parseOperand ('X':'\'':xs) = flip Operand OpImmediate . Left . bytesToInteger <$> (quoted ('\'':xs) >>= hexstring)
parseOperand xs            = (flip Operand OpSimple) <$> (choice numeric alphaNumeric xs)

--
-- General parsers
--

-- | Parses a string in quotes: i.e.
-- the first char is a '\'', as is the last.
-- Excludes the quotes in the result.
quoted :: Token -> Result String
quoted = (=<<) f . nonnull
  where f ('\'':xs)
          | after == "'" = Right before
          | otherwise = Left "no closing quote"
          where (before,after) = span ((/=) '\'') xs
        f _ = Left "no opening quote"

-- | Parses a string that has length > 0.
nonnull :: Token -> Result String
nonnull "" = Left "expected non-null string"
nonnull str = Right str

-- | Parses numeric strings.
numeric :: Token -> Result Integer
numeric = (=<<) readEither . predicated isNumber

-- | Parses alphanumeric strings.
alphaNumeric :: Token -> Result String
alphaNumeric = predicated isAlphaNum

-- | Makes a parser from a predicate such that:
-- 1. For all characters c in the result, p(c(sub)i) = True
-- 2. The length of the result must equal the length of the input token.
predicated :: (Char -> Bool) -> Token -> Result String
predicated p str = nonnull =<< sequence (unfoldr f str)
  where f [] = Nothing
        f (x:xs)
          | p x = Just (Right x, xs) 
          | otherwise = Just (Left $ "unexpected '" ++ (x:"'") , "")

-- | Makes a parser that will succeed if one of @l@ and @r@ succeeds.
choice ::  (a -> Result l) -> (a -> Result r) -> a -> Result (Either l r)
choice a b str = f (a str) (b str)
  where
    f (Right l)  _        = Right (Left l)
    f (Left _)  (Right r) = Right (Right r)
    f (Left el) (Left er) = Left $ el ++ " / " ++ er

--
-- Specific parsers
--

-- TODO: nonnull????
-- | Reads a binary value in hex from a 'String'.
hexstring :: String -> Result [Word8]
hexstring = fmap (groupN []) . mapM (hexchar . toUpper)
  where groupN acc [] = acc
        groupN acc (x:[]) = acc ++ [x]
        groupN acc (a:b:xs) = groupN (acc ++ [b + (a `shiftL` 4)]) xs

-- | Reads a hex nybble into the low bits of a byte
-- from a 'Char'.
hexchar :: Char -> Result Word8
hexchar a
  | between '0' '9' a = Right $ fromIntegral $ ord a - ord '0'
  | between 'A' 'F' a = Right $ fromIntegral $ (ord a - ord 'A') + 10
  | otherwise = Left $ "unexpected '" ++ (a:"'")
  where between x y c = ord y >= ord c && ord c >= ord x

