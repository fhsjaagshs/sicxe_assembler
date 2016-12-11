{-|

-}

module Assembler
(
  assemble,
  packBits,
  toBits
)
where

import Common
import Parser
import Definitions
import Data.Word
import Data.Bits
import Data.Bool

import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM

import Data.Maybe
import Data.Foldable
import Data.Monoid
import Control.Applicative
import Control.Monad
import Control.Monad.State.Lazy

assemble :: [Line] -> Maybe [[Word8]]
assemble ls = runAssembler 0 mempty $ do
  firstPass ls
  resetAddress
  secondPass ls

type Address = Word32 -- | 1MB Address
type SymbolTable = HashMap String Address -- | Symbol table
type Assembler a = State (Address, SymbolTable) a -- | Assembler monad

-- | Run your assembler.
runAssembler :: Address -> SymbolTable -> Assembler a -> a
runAssembler a st ass = evalState ass (a, st)

-- | Get the current address.
address :: Assembler Address
address = fst <$> get

-- | Set the current address 
setAddress :: Address -> Assembler ()
setAddress addr = state $ \(_, st) -> ((), (addr, st))

-- | Set the current address to the start of the program.
-- Use this instead of @setAddress 0@ because 'Assembler'
-- may change to support START and END better.
resetAddress :: Assembler ()
resetAddress = setAddress 0

-- | Advance the current address by @by@
advanceAddress :: Address -> Assembler ()
advanceAddress by = address >>= setAddress . (+) by

-- | The symbol table.
symbolTable :: Assembler SymbolTable
symbolTable = snd <$> get

-- | Sets a symbol in the symbol table
setSymbol :: String -> Address -> Assembler ()
setSymbol sym a = state $ \(addr, st) -> ((), (addr, HM.insert sym a st))

-- | Gets a symbol from the symbol table
getSymbol :: String -> Assembler (Maybe Address)
getSymbol sym = HM.lookup sym <$> symbolTable

--
-- First Pass
--

-- | Does the first pass of assembly, finding all of
-- the labels and recording them in the symtab.
firstPass :: [Line] -> Assembler ()
firstPass xs = f xs
  where
    f [] = return ()
    f (l@(Line mlbl _ _):ls) = do
      case mlbl of
        Just lbl -> address >>= setSymbol lbl
        Nothing -> return ()
      ms <- sizeofLine l
      case ms of
        Just s -> advanceAddress s >> f ls
        Nothing -> return ()

--
-- Second Pass (High Level)
--

-- | Does the second pass of assembly, assembling
-- all of the lines of assembly code into object code 
secondPass :: [Line] -> Assembler (Maybe [[Word8]])
secondPass ls = sequence <$> mapM assembleLine ls

-- | Determine the format of a line of SIC/XE assembler.
lineFormat :: Line -> Assembler (Maybe Int)
lineFormat (Line _ (Mnemonic m extended) oprs) = maybe (return Nothing) f $ lookupMnemonic m
  where
    f = findM (valid oprs) . opdescFormats
    -- | @valid@ is a predicate that validates
    -- the line's operands with regard to the
    -- instruction format(s) dictated by the mnemonic. 
    valid []     1 = return True
    valid (x:xs) 2 = return $ and $ map (isJust . simpleToByte) (x:xs)
    valid (x:_)  3
      | extended = return False
      | reqAbs x = return True
      | otherwise = do
        addrc <- address
        addrx <- fromMaybe addrc <$> getAddr x
        let disp = ((fromIntegral addrx) - (fromIntegral addrc)) :: Integer
        return $ not $ disp < -2048 || disp > 4095
    valid (_:_)  4 = return True
    valid _      _ = return False

-- | Determine the size of a line (directive or instruction) of SIC/XE
-- assembler code without accessing the symbol table or assembling code.
sizeofLine :: Line -> Assembler (Maybe Word32)
sizeofLine l@(Line _ (Mnemonic m _) oprs) = do
  lf <- lineFormat l
  return $ (fromIntegral <$> lf) <|>  ds m oprs
  where
    ds :: String -> [Operand] -> Maybe Word32
    ds "BYTE" [Operand (Left v) OpImmediate] = Just $ fromIntegral $ length $ integerToBytes v
    ds "WORD" [Operand (Left v) OpSimple] = Just 3
    ds "RESB" [Operand (Left n) OpSimple] = Just $ fromIntegral n
    ds "RESW" [Operand (Left n) OpSimple] = Just $ 3 * (fromIntegral n)
    ds "START" [Operand (Left n) OpSimple] = Just $ fromIntegral n
    ds _ _ = Nothing

-- | Assembles a line of SIC/XE ASM as parsed by Parser.
-- Returns a list of bytes in Big Endian order and the next address.
assembleLine :: Line -> Assembler (Maybe [Word8])
assembleLine l@(Line _ (Mnemonic m _) oprs) = do
  lf <- lineFormat l
  g $ (,) <$> lf <*> lookupMnemonic m
  where
    g (Just (f, OpDesc opc _ _)) = mkinstr opc f oprs
    g Nothing                    = mkdirec m oprs
    mkinstr :: Word8 -> Int -> [Operand] -> Assembler (Maybe [Word8])
    mkinstr opc 1 _      = Just <$> format1 opc
    mkinstr opc 2 [a]    = mayapply (format2 opc) (simpleToByte a) (Just 0)
    mkinstr opc 2 [a, b] = mayapply (format2 opc) (simpleToByte a) (simpleToByte b)
    mkinstr opc 3 [a, b] = getAddr a >>= mayapply (format3 (reqAbs a) opc (getN a) (getI a)) (return $ getX a b)
    mkinstr opc 3 [a]    = getAddr a >>= mayapply (format3 (reqAbs a) opc (getN a) (getI a)) (return False)
    mkinstr opc 4 [a, b] = getAddr a >>= mayapply (format4 opc (getN a) (getI a)) (return $ getX a b)
    mkinstr opc 4 [a]    = getAddr a >>= mayapply (format4 opc (getN a) (getI a)) (return False)
    mkdirec "BYTE" [Operand (Left v) OpImmediate] = Just <$> byte v
    mkdirec "WORD" [Operand (Left v) OpSimple] = Just <$> word v
    mkdirec "RESB" [Operand (Left n) OpSimple] = Just <$> resb (fromIntegral n)
    mkdirec "RESW" [Operand (Left n) OpSimple] = Just <$> resw (fromIntegral n)
    mkdirec "START" [Operand (Left n) OpSimple] = Just <$> start (fromIntegral n)
    mkdirec _ _ = return Nothing

-- | Calculates the absolute address contained in an operand.
getAddr :: Operand -> Assembler (Maybe Address)
getAddr (Operand (Left s) _) = return $ Just $ fromIntegral s
getAddr (Operand (Right s) _) = getSymbol s
    
--
-- Helpers
--

-- | Determines if @operand@ is the index register.
isIndexingReg :: Operand -> Bool
isIndexingReg (Operand (Right v) OpSimple) = v == fst indexingRegister
isIndexingReg _ = False

reqAbs :: Operand -> Bool
reqAbs (Operand (Left _) OpImmediate) = True
reqAbs _                              = False

-- | Determines if @operand@ is a given 'OperandType'
isType :: OperandType -> Operand -> Bool
isType t2 (Operand _ t) = t == t2

-- | DRY to calculate X of nixbpe.
getX :: Operand -> Operand -> Bool
getX a b = isIndexingReg b && isType OpSimple a

-- | DRY to calculate I of nixbpe
getI :: Operand -> Bool
getI a = isType OpImmediate a || isType OpSimple a

-- | DRY to calculate N of nixbpe
getN :: Operand -> Bool
getN a = isType OpIndirect a || isType OpSimple a

-- | Looks up an opcode from the 'Descriptions' module.
lookupMnemonic :: String -> Maybe OpDesc
lookupMnemonic m = find ((==) m . opdescMnemonic) operations

-- | Turns an operand into either a register code or its integral value.
simpleToByte :: Operand -> Maybe Word8
simpleToByte (Operand (Right ident) OpSimple) = lookup ident registers
simpleToByte (Operand (Left i) OpSimple) = Just $ fromIntegral i
simpleToByte _ = Nothing

--
-- Second Pass (Low Level)
--

-- | Assembles a 24-bit word constant
word :: Integer -> Assembler [Word8]
word i = do
  advanceAddress 3
  return $ packBits $ toBits ((fromIntegral i) :: Word32)

-- | Assembles a binary constant
byte :: Integer -> Assembler [Word8]
byte bs = do
  advanceAddress $ fromIntegral $ length bs'
  return bs'
  where bs' = integerToBytes bs

-- | Reserve bytes of space.
resb :: Word32 -> Assembler [Word8]
resb i = do
  advanceAddress i
  return $ replicate (fromIntegral i) 0x0

-- | Reserve words of space.
resw :: Word32 -> Assembler [Word8]
resw = resb . (* 3)

-- | Start directive.
start :: Word32 -> Assembler [Word8]
start = resb

-- | Assembles a Format 1 instruction.
format1 :: Word8 -> Assembler [Word8]
format1 w = do
  advanceAddress 1
  return [w]

-- | Assembles a Format 2 instruction.
format2 :: Word8 -> Word8 -> Word8 -> Assembler [Word8]
format2 op rega regb = do
  advanceAddress 2
  return [op, shiftL rega 4 .|. regb]

-- | Assembles a Format 3 instruction.
format3 :: Bool -> Word8 -> Bool -> Bool -> Bool -> Word32 -> Assembler [Word8]
format3 absolute op n i x memoff = do
  curaddr <- address
  let disp = ((fromIntegral curaddr) - (fromIntegral memoff)) :: Int
      b = disp >= 0 && disp <= 4095
      p = disp >= -2048 && disp <= 2047
      prefix = format34DRY op n i x b p False
  if b || p
    then do
      advanceAddress 3
      let disp' = bool disp (fromIntegral memoff) absolute
      return $ packBits $ prefix ++ take 12 (toBits disp')
    else format4 op n i x memoff

-- | Assembles a Format 4 instruction.
format4 :: Word8 -> Bool -> Bool -> Bool -> Word32 -> Assembler [Word8]
format4 op n i x addr = do
  advanceAddress 4
  return $ packBits $ prefix ++ addr'
  where
    prefix = format34DRY op n i x False False True
    addr' = take 20 $ toBits addr

format34DRY :: Word8 -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> [Bool]
format34DRY op n i x b p e = toBits (set (set op 0 i) 1 n) ++ [x, b, p, e]
  where set v i True = setBit v i
        set v i False = clearBit v i

-- | Turns a data structure representing bits into a list of bits in bit endian order,
-- ignoring bit ordering.
-- 'toBits' is dedicated to Fritz Wiedmer, my grandfather (~1925 to 2016). During
-- his time at IBM, he designed Bubbles memory and ECC for keyboards. Fritz is the
-- reason I became fascinated with computer science.
toBits :: FiniteBits a => a -> [Bool]
toBits x = map (testBit x . uncurry (+) . idx2bitbyte) [0..bitlen - 1]
  where bitlen = finiteBitSize x
        idx2bitbyte idx = (7 - mod idx 8, div idx 8)

-- | Packs a list of bits into a list of bytes
-- in machine order
packBits :: [Bool] -> [Word8]
packBits = f []
  where
    f acc [] = acc
    f acc xs = f (acc ++ [b]) xs'
      where
        (bits, xs') = splitAt 8 xs
        bits' = bits ++ replicate (max (8 - length bits) 0) False
        indeces = [7,6,5,4,3,2,1,0]
        b = foldl (+) 0 $ map (uncurry bit') $ zipWith (,) indeces bits'
    bit' i True = bit i
    bit' i False = zeroBits

