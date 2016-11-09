module Instruction
(
  Instruction(..),
  Flags(..),
  Opcode,
  RelativeAddress
  AbsoluteAddress
)

import Data.ByteString (ByteString)
import Data.Binary
import Data.Word
import Data.Bits

data Flags = {
  flagsN :: !Bool,
  flagsI :: !Bool,
  flagsX :: !Bool,
  flagsB :: !Bool,
  flagsP :: !Bool,
  flagsE :: !Bool
}

flagsToByte :: Flags -> Word8
flagsToByte (Flags n i x b p e) = s n 0 $ s i 1 $ s x 2 $ s b 3 $ s p 4 $ s e 5 zeroBits
  where s b i a = (b ? setBit : clearBit) a i

byteToFlags :: Word8 -> Flags
byteToFlags b = Flags (t 0) (t 1) (t 2) (t 3) (t 4) (t 5) 
  where t i = testBit b i 

type Opcode = Word8
type RelativeAddress = Word16 -- 0 <= disp <= 4095
type AbsoluteAddress = Word32 -- -2048 <= disp <= 2047
data Instruction = Format1 Opcode | Format2 Opcode Flags | Format3 Opcode Flags RelativeAddress | Format4 Word8 Flags AbsoluteAddress

-- TODO: binary

getOpcode :: ByteString -> Maybe Word8
getOpcode "ADD"    = Just 0x18
getOpcode "ADDR"   = Just 0x90
getOpcode "AND"    = Just 0x40
getOpcode "CLEAR"  = Just 0xB4
getOpcode "COMP"   = Just 0x28
getOpcode "COMPR"  = Just 0xa0
getOpcode "DIV"    = Just 0x24
getOpcode "DIVR"   = Just 0x9C
getOpcode "HIO"    = Just 0xF4
getOpcode "J"      = Just 0x3C
getOpcode "JEQ"    = Just 0x30
getOpcode "JGT"    = Just 0x34
getOpcode "JLT"    = Just 0x38
getOpcode "JSUB"   = Just 0x48
getOpcode "LDA"    = Just 0x00
getOpcode "LDB"    = Just 0x68
getOpcode "LDCH"   = Just 0x50
getOpcode "LDF"    = Just 0x70
getOpcode "LDL"    = Just 0x08
getOpcode "LDS"    = Just 0x6C
getOpcode "LDT"    = Just 0x74
getOpcode "LDX"    = Just 0x04
getOpcode "LPS"    = Just 0xD0
getOpcode "MUL"    = Just 0x20
getOpcode "MULR"   = Just 0x98
getOpcode "OR"     = Just 0x44
getOpcode "RD"     = Just 0xD8
getOpcode "RMO"    = Just 0xAC
getOpcode "RSUB"   = Just 0x4C
getOpcode "SHIFTL" = Just 0xA4
getOpcode "SHIFTR" = Just 0xA8
getOpcode "SIO"    = Just 0xF0
getOpcode "SSK"    = Just 0xEC
getOpcode "STA"    = Just 0x0C
getOpcode "STB"    = Just 0x78
getOpcode "STCH"   = Just 0x54
getOpcode "STI"    = Just 0xD4
getOpcode "STL"    = Just 0x14
getOpcode "STS"    = Just 0x7C
getOpcode "STSW"   = Just 0xE8
getOpcode "STT"    = Just 0x84
getOpcode "STX"    = Just 0x10
getOpcode "SUB"    = Just 0x1C
getOpcode "SUBR"   = Just 0x94
getOpcode "SVC"    = Just 0xB0
getOpcode "TD"     = Just 0xE0
getOpcode "TIO"    = Just 0xF8
getOpcode "TIX"    = Just 0x2C
getOpcode "TIXR"   = Just 0xB8
getOpcode "WD"     = Just 0xDC
getOpcode _ = Nothing
