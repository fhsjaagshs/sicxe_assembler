module Symbols
(
  symbolTable
)
where

import Parser
import Assembler

-- Maps a symbol label to an address
symbolTable :: [Line] -> [(String, Word32)]
symbolTable ls = [] -- TODO: implement!!!!
