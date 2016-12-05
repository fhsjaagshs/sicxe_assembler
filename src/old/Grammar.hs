{-# LANGUAGE OverloadedStrings #-}

module Grammar
(

)
where

import Instruction

import Data.ByteString (ByteString)
import qualified Data.ByteString as B

type Arguments = []

-- Two types of lines in SIG/XE:
-- ones that start with a period
-- ones that are 3-columed split by whitespace
--   -> | LABEL | INSTRUCTION | WHITESPACE |

-- TODO: parsers for
{-
  - 1. Parser directives
  - 2. arguements
-}



