{-|
Module      : Core
Description : Basic operations around parsing SIG/XE instructions, and data declarations.
Copyright   : (c) Nathaniel Symer, 2016
License     : MIT
Maintainer  : nate@symer.io
Stability   : stable
Portability : POSIX
-}

{-# LANGUAGE OverloadedStrings  #-}

module Core
(

)
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B

-- TODO: describe instructions via DSL

data Format = Format1 | Format2 | Format3 | Format4 deriving (Eq, Show)
data Argument = ArgumentImmediate | ArgumentIndirect | ArgumentDirect deriving (Eq, Show)
data Instruction = Instruction {
  instructionMnemonic :: ByteString,
  instructionOpcode   :: !Word8,
  instructionFormat   :: Maybe Format
  instructionArgumentFormats :: [[Argument]],
} deriving (Eq, Show) -- TODO: Ensure I'm not missing anything here

-- If @Instruction@ is a description of what's being parsed,
-- what is the result of parsing?

-- TODO: implement
parseInstruction :: ByteString -> Either String Instruction
parseInstruction bs = Right $ Instruction "" 0 Nothing []

data DataDeclaration = DataDeclaration {
  dataDeclarationName :: ByteString,
  dataDeclarationSizeInBytes :: !Word8,
  dataDeclarationIsVariable :: !Bool,
  dataDeclarationDeclaration :: ByteString
} deriving (Eq, Show)

-- TODO: implement
parseDataDeclaration :: ByteString -> DataDeclaration
