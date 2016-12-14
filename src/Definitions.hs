{-|
Module: Definitions
Description: Definites instruction sets for use with Assembler
-}

module Definitions
(
  OpDesc(..),
  registers,
  indexingRegister,
  operations
)
where

import Common
import Parser

import AccumulatorT
import Data.Functor.Identity
import Data.List
import Data.Word
import Data.Maybe

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

-- | Simple DRY to make running an AccumulatorT less annoying.
accum :: i -> (i -> c) -> AccumulatorT c i Identity () -> [c]
accum i f acc = res
  where
    (_, res, _) = runIdentity $ evalAccumulatorT acc (return . f) (return i)

-- | Instruction set's registers
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
-- an instruction set with an indexing register.
indexingRegister :: (String, Word8)
indexingRegister = ("X", 1)

-- | Description of an operation. Includes information
-- about mnemonics & operands.
data OpDesc = OpDesc {
  opdescOpcode :: Word8,
  opdescMnemonic :: String,
  opdescFormats :: [Int],
  opdescNumberOperands :: Int,
  opdescOperandTransform :: Operand -> Operand,
  opdescOperandValidators :: HashMap Int (Operand -> Int -> Bool)
}

-- | Sort an 'OpDesc''s formats (Assembler.hs expects this).
sortFormats :: OpDesc -> OpDesc
sortFormats o = o { opdescFormats = (sort $ opdescFormats o) }

-- | Makes an 'Operand' validator from a list of prediates.
packV :: [Operand -> Bool] -> Operand -> Int -> Bool
packV xs op i = maybe False ($ op) (safeIdx i xs)

--
-- OpDesc operand validators
--

isRegister :: Operand -> Bool
isRegister (Operand (Right ident) OpSimple) = isJust $ lookup ident registers
isRegister _                                = False

isMemory :: Operand -> Bool
isMemory = not . isRegister

isMemoryOrImmediate :: Operand -> Bool
isMemoryOrImmediate o@(Operand (Right _) _) = not $ isRegister o
isMemoryOrImmediate (Operand (Left _) t) = t == OpImmediate 

isNumber :: Operand -> Bool
isNumber (Operand (Left _) OpSimple) = True
isNumber _                           = False

singleMemory :: Operand -> Int -> Bool
singleMemory = packV [isMemory]

twoRegisters :: Operand -> Int -> Bool
twoRegisters = packV [isRegister, isRegister]

noOperands :: Operand -> Int -> Bool
noOperands = const $ const False

singleMemOrImm :: Operand -> Int -> Bool
singleMemOrImm = packV [isMemoryOrImmediate]

--
-- Operation Declarations
--

operations :: [OpDesc]
operations = accum (OpDesc 0x00 "" [] 0 id mempty) sortFormats $ do
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
    numberOperands 1
    validator 2 $ packV [isRegister] 
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
    validator 3 singleMemOrImm
    validator 4 singleMemOrImm
  op "LDB" $ do 
    opcode 0x68
    format 3
    format 4
    validator 3 singleMemOrImm
    validator 4 singleMemOrImm
  op "LDCH" $ do 
    opcode 0x50
    format 3
    format 4
    validator 3 singleMemOrImm
    validator 4 singleMemOrImm
  op "LDL" $ do
    opcode 0x08
    format 3
    format 4
    validator 3 singleMemOrImm
    validator 4 singleMemOrImm
  op "LDS" $ do
    opcode 0x6C
    format 3
    format 4
    validator 3 singleMemOrImm
    validator 4 singleMemOrImm
  op "LDT" $ do
    opcode 0x74
    format 3
    format 4
    validator 3 singleMemOrImm
    validator 4 singleMemOrImm
  op "LDX" $ do
    opcode 0x04
    format 3
    format 4
    validator 3 singleMemOrImm
    validator 4 singleMemOrImm
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
    numberOperands 0
  op "SHIFTL" $ do
    opcode 0xA4
    format 2
    transform $ \o -> if isNumber o then decr o else o
    validator 2 $ packV [isRegister, isNumber]
  op "SHIFTR" $ do
    opcode 0xA8
    format 2
    transform $ \o -> if isNumber o then decr o else o
    validator 2 $ packV [isRegister, isNumber]
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
    numberOperands 1
    validator 2 $ packV [isNumber]
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
    numberOperands 1
    validator 2 $ packV [isRegister]
  op "WD" $ do
    opcode 0xDC
    format 3
    format 4
  where
    op m act = mnemonic m >> act >> complete
    opcode o = incomplete $ \(OpDesc _ m fs n f v) -> return $ OpDesc o m fs n f v
    mnemonic m = incomplete $ \(OpDesc o _ fs n f v) -> return $ OpDesc o m fs n f v
    format fmt = incomplete $ \(OpDesc o m fs _ f v) -> return $ OpDesc o m (fs ++ [fmt]) (getMNumOps fmt) f $ HM.insert fmt (getValidator fmt) v
      where getMNumOps 4 = 1
            getMNumOps 3 = 1
            getMNumOps 2 = 2
            getMNumOps 1 = 0
            getMNumOps _ = 0
            getValidator 4 = singleMemory
            getValidator 3 = singleMemory
            getValidator 2 = twoRegisters
            getValidator 1 = noOperands
            getValidator _ = noOperands
    numberOperands n = incomplete $ \(OpDesc o m fs _ f v) -> return $ OpDesc o m fs n f v
    validator fmt v = incomplete $ \(OpDesc o m fs n f vs) -> return $ OpDesc o m fs n f $ HM.insert fmt v vs
    transform f = incomplete $ \(OpDesc o m fs n _ v) -> return $ OpDesc o m fs n f v
    decr (Operand (Left v) OpSimple) = Operand (Left $ v - 1) OpSimple
    decr o = o
