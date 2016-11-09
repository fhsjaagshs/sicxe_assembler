{-# LANGUAGE OVerloadedStrings #-}

module Grammar
(

)
where

import Instruction

import Data.Attoparsec.ByteString.Char8 as A
import Data.ByteString (ByteString)
import qualified Data.ByteString as B

line :: Parser (ByteString,ByteString,ByteString)
line = (,,) <$> section <*> section <*> A.takeUntil (\c -> c == '\n')
  where
    section = A.takeWhile (not . whitespace) <* A.skipWhile whitespace
    whitespace ' ' = True
    whitespace _ = false
