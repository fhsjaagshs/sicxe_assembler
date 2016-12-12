module Definitions
(
  OpDesc(..),
  registers,
  indexingRegister,
  operations
)
where

import AccumulatorT
import Data.Functor.Identity
import Data.List
import Data.Word

accum :: i -> (i -> c) -> AccumulatorT c i Identity () -> [c]
accum i f acc = res
  where
    (_, res, _) = runIdentity $ evalAccumulatorT acc (return . f) (return i)

registers :: [(String, Word8)]
registers  = accum ("", 0) id $ do
  register 0 "A"
  register 1 "X"
  register 2 "L"
  register 3 "B"
  register 4 "S"
  register 5 "T"
  register 8 "PC"
  register 9 "SW"
  where
    register n str = setIncomplete (str, n) >> complete

-- NOTE: I wish there were a more abstract way to
-- define this behavior so that this module doesn't
-- limit the assembler to only being able to assemble
-- an assembly language with an indexing register.
indexingRegister :: (String, Word8)
indexingRegister = ("X", 1)

data OpDesc = OpDesc {
  opdescOpcode :: Word8,
  opdescMnemonic :: String,
  opdescFormats :: [Int]
} deriving (Eq, Show)

sortFormats :: OpDesc -> OpDesc
sortFormats (OpDesc o m fs) = OpDesc o m (sort fs)

operations :: [OpDesc]
operations = accum (OpDesc 0x00 "" []) sortFormats $ do
  op "ADD" $ do
    opcode 0x18
    format 3
    format 4
  op "ADDR" $ do 
    opcode 0x90
    format 2
  op "AND" $ do
    opcode 0x40
    format 3
    format 4
  op "CLEAR" $ do
    opcode 0xB4
    format 2
  op "COMP" $ do
    opcode 0x28
    format 3
    format 4
  op "COMPR" $ do
    opcode 0xA0
    format 2
  op "DIV" $ do
    opcode 0x24
    format 3
    format 4
  op "DIVR" $ do
    opcode 0x9C
    format 2
  op "HIO" $ do
    opcode 0xF4
    format 1
  op "J" $ do
    opcode 0x3C
    format 3
    format 4
  op "JEQ" $ do
    opcode 0x30
    format 3
    format 4
  op "JGT" $ do
    opcode 0x34
    format 3
    format 4
  op "JLT" $ do 
    opcode 0x38
    format 3
    format 4
  op "JSUB" $ do 
    opcode 0x48
    format 3
    format 4
  op "LDA" $ do 
    opcode 0x00
    format 3
    format 4
  op "LDB" $ do 
    opcode 0x68
    format 3
    format 4
  op "LDCH" $ do 
    opcode 0x50
    format 3
    format 4
  op "LDL" $ do
    opcode 0x08
    format 3
    format 4
  op "LDS" $ do
    opcode 0x6C
    format 3
    format 4
  op "LDT" $ do
    opcode 0x74
    format 3
    format 4
  op "LDX" $ do
    opcode 0x04
    format 3
    format 4
  op "LPS" $ do
    opcode 0xD0 
    format 3
    format 4
  op "MUL" $ do
    opcode 0x20
    format 3
    format 4
  op "MULR" $ do
    opcode 0x98
    format 2
  op "OR" $ do
    opcode 0x44
    format 3
    format 4
  op "RD" $ do
    opcode 0xD8
    format 3
    format 4
  op "RMO" $ do
    opcode 0xAC
    format 2
  op "RSUB" $ do
    opcode 0x4C
    format 3
    format 4
  op "SHIFTL" $ do
    opcode 0xA4
    format 2
  op "SHIFTR" $ do
    opcode 0xA8
    format 2
  op "SIO" $ do
    opcode 0xF0 
    format 1
  op "SSK" $ do
    opcode 0xEC
    format 3
    format 4
  op "STA" $ do
    opcode 0x0C
    format 3
    format 4
  op "STB" $ do
    opcode 0x78
    format 3
    format 4
  op "STCH" $ do
    opcode 0x54
    format 3
    format 4
  op "STI" $ do
    opcode 0xD4
    format 3
    format 4
  op "STL" $ do
    opcode 0x14
    format 3
    format 4
  op "STS" $ do
    opcode 0x7C
    format 3
    format 4
  op "STSW" $ do
    opcode 0xE8
    format 3
    format 4
  op "STT" $ do
    opcode 0x84
    format 3
    format 4
  op "STX" $ do
    opcode 0x10
    format 3
    format 4
  op "SUB" $ do
    opcode 0x1C
    format 3
    format 4
  op "SUBR" $ do
    opcode 0x94
    format 2
  op "SVC" $ do
    opcode 0xB0
    format 2
  op "TD" $ do
    opcode 0xE0
    format 3
    format 4
  op "TIO" $ do
    opcode 0xF8
    format 1
  op "TIX" $ do
    opcode 0x2C
    format 3
    format 4
  op "TIXR" $ do
    opcode 0xB8
    format 2
  op "WD" $ do
    opcode 0xDC
    format 3
    format 4
  where
    op m act = mnemonic m >> act >> complete
    opcode o = incomplete $ \(OpDesc _ m fs) -> return $ OpDesc o m fs
    mnemonic m = incomplete $ \(OpDesc o _ fs) -> return $ OpDesc o m fs
    format f = incomplete $ \(OpDesc o m fs) -> return $ OpDesc o m (fs ++ [f])

