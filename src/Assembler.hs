{-|
 -
-}

{-# #-}

module Assembler
(
  sizeofLine,
  assembleLine
)
where

import Parser
import Buffer

sizeofLine :: Line -> Int 
sizeofLine (LDirective d) = sizeofDirective d
sizeofLine (LInstruction i) = sizeofInstruction i 

sizeofDirective :: Directive -> Int
sizeofDirective (Directive _ START (Left cnt)) = 
sizeofDirective (Directive _ START (Right imm))

sizeofInstruction :: Instruction -> Int

-- TODO: implement assembly
assembleLine :: Line -> IO Buffer
assembleLine (LDirective d) = assembleDirective d
assembleLine (LInstruction i) = assembleInstruction i

assembleDirective :: Directive -> IO Buffer

assembleInstruction :: Instruction -> IO Buffer
