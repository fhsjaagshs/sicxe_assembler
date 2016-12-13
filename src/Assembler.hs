{-# LANGUAGE ScopedTypeVariables #-}

module Assembler
(
  assemble,
  isBaseRelative,
  isPCRelative,
  calcDisp,
  toBits
)
where

import Common
import Parser
import Definitions
import Data.Word
import Data.Int
import Data.Bits
import Data.Bool
import Data.List

import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM

import Data.Maybe
import Data.Foldable
import Data.Monoid
import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Monad.State.Strict

assemble :: [Line] -> Result [[Word8]]
assemble = (=<<) f . mapM preprocessLine
  where
    f ls = runAssembler 0 mempty $ do
      firstPass ls
      resetAddress
      secondPass ls

type Address = Word32 -- | 1MB Address
type SymbolTable = HashMap String Address -- | Symbol table
type Assembler a = State (Address, SymbolTable, Maybe Address) a -- | Assembler monad

-- | Run your assembler.
runAssembler :: Address -> SymbolTable -> Assembler a -> a
runAssembler a st ass = evalState ass (a, st, Nothing)

-- | Get the current address.
address :: Assembler Address
address = fst' <$> get

-- | Set the current address 
setAddress :: Address -> Assembler ()
setAddress addr = state $ \(_, st, b) -> ((), (addr, st, b))

getBase :: Assembler (Maybe Address)
getBase = thd' <$> get

setBase :: Address -> Assembler ()
setBase a = state $ \(addr, st, b) -> ((), (addr, st, b <|> Just a))

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
symbolTable = snd' <$> get

-- | Sets a symbol in the symbol table
setSymbol :: String -> Address -> Assembler ()
setSymbol sym a = state $ \(addr, st, b) -> ((), (addr, HM.insert sym a st, b))

-- | Gets a symbol from the symbol table
getSymbol :: String -> Assembler (Result Address)
getSymbol sym = fromM err . HM.lookup sym <$> symbolTable
  where err = "symbol '" ++ sym ++ "' doesn't exist"

--
-- First Pass
--

-- | Does the first pass of assembly, finding all of
-- the labels and recording them in the symtab.
firstPass :: [Line] -> Assembler (Result ())
firstPass [] = return $ Right ()
firstPass (Line _ (Mnemonic "BASE" False) _:ls) = (address >>= setBase) >> firstPass ls
firstPass (l@(Line lbl _ _):ls) = do
  applyResultA (\s -> setSymbol s =<< address) $ fromM "" lbl
  lsz <- sizeofLine l
  case lsz of
    Right v -> do
      advanceAddress v
      firstPass ls
    Left e -> return $ Left e
--
-- Second Pass (High Level)
--

-- | Does the second pass of assembly, assembling
-- all of the lines of assembly code into object code 
secondPass :: [Line] -> Assembler (Result [[Word8]])
secondPass = fmap sequence . mapM assembleLine

-- TODO: implement numberOperands (with special concerns for ,X), transforms, and validators

-- | Tests if a 'Line' is valid.
preprocessLine :: Line -> Result Line
preprocessLine l@(Line lbl (Mnemonic m extended) oprs)
  | m `elem` ["BYTE", "WORD", "RESB", "RESW", "START", "END", "BASE"] = Right l
  | otherwise = lookupMnemonic m >>= f
  where
    f (OpDesc _ _ fs no xform validator)
      | not hasEnoughOperands = Left "not enough operands"
      | (not $ elem 4 fs) && extended = Left "non extensible mnemonic extended"
     -- | not operandsMatch = Left "invalid operands"
      | otherwise = Right $ Line lbl (Mnemonic m extended) oprs''
      where
        hasEnoughOperands = (length oprs) >= no
        operandsMatch = all (uncurry validator) $ zipWith (,) oprs [0..no - 1] 
        oprs' = take no oprs
        oprs''
          | fromMaybe False $ (< no) <$> idxRegIdx = map xform oprs'
          | otherwise = map xform $ oprs' ++ (maybe [] pure $ find isIndexingReg oprs)
          where
            idxRegIdx = findIndex isIndexingReg oprs
    --    hasIndexingOp = (==) 1 . length . findIndices isIndexingReg

-- | Determine the format of a line of SIC/XE assembler.
lineFormat :: Line -> Assembler (Result Int)
lineFormat (Line _ (Mnemonic m extended) oprs) = either (return . Left) f $ lookupMnemonic m
  where
    f = (>>= toRes) . findM (valid oprs) . opdescFormats
    toRes mlf = do
      a <- address
      return $ fromM ("invalid line at address: " ++ show a) mlf
    -- | @valid@ is a predicate that validates 
    -- the line's operands with regard to the
    -- instruction format(s) dictated by the mnemonic. 
    valid []     1 = return True
    valid (x:xs) 2 = return $ and $ map (isType OpSimple) (x:xs)
    valid (x:_)  3
      | extended = return False
      | reqAbs x = return True
      | otherwise = do
        addrc <- address
        addrx <- either (const addrc) id <$> getAddr x
        let disp = (fromIntegral addrc) - (fromIntegral addrx)
        return $ disp >= -2048 || disp < 4096
    valid _      3 = return True
    valid _      4 = return True
    valid _      _ = return False

-- | Determine the size of a line (directive or instruction) of SIC/XE
-- assembler code without accessing the symbol table or assembling code.
sizeofLine :: Line -> Assembler (Result Word32)
sizeofLine l@(Line _ (Mnemonic m _) oprs) = do
  lf <- lineFormat l
  return $ (fromIntegral <$> lf) <|>  ds m oprs
  where
    ds :: String -> [Operand] -> Result Word32
    ds "BYTE" [Operand (Left v) OpImmediate] = Right $ fromIntegral $ length $ integerToBytes v
    ds "WORD" [Operand (Left v) OpSimple] = Right 3
    ds "RESB" [Operand (Left n) OpSimple] = Right $ fromIntegral n
    ds "RESW" [Operand (Left n) OpSimple] = Right $ 3 * (fromIntegral n)
    ds "START" [Operand (Left n) OpSimple] = Right $ fromIntegral n
    ds "END" _ = Right 0
    ds "BASE" _ = Right 0
    ds mp _ = Left $ ('\'':mp) ++ "' is not a directive nor a mnemonic"

-- | Assembles a line of SIC/XE ASM as parsed by Parser.
-- Returns a list of bytes in Big Endian order and the next address.
assembleLine :: Line -> Assembler (Result [Word8])
assembleLine l@(Line _ (Mnemonic m _) oprs) = do
  lf <- lineFormat l
  g $ (,) <$> lf <*> lookupMnemonic m
  where
    g (Right (f, o)) = mkinstr (opdescOpcode o) f oprs
    g (Left _)       = mkdirec m oprs
    mkinstr :: Word8 -> Int -> [Operand] -> Assembler (Result [Word8])
    mkinstr opc 1 _      = Right <$> format1 opc
    mkinstr opc 2 [a]    = applyResultA2 (format2 opc) (format2Operand a) (Right 0)
    mkinstr opc 2 [a, b] = applyResultA2 (format2 opc) (format2Operand a) (format2Operand b)
    mkinstr opc 3 [a, b] = getAddr a >>= applyResultA2 (format3 (reqAbs a) opc (getN a) (getI a)) (return $ getX a b)
    mkinstr opc 3 [a]    = getAddr a >>= applyResultA2 (format3 (reqAbs a) opc (getN a) (getI a)) (return False)
    mkinstr opc 3 []     = Right <$> format3 True opc True True False 0
    mkinstr opc 4 [a, b] = getAddr a >>= applyResultA2 (format4 opc (getN a) (getI a)) (return $ getX a b)
    mkinstr opc 4 [a]    = getAddr a >>= applyResultA2 (format4 opc (getN a) (getI a)) (return False)
    mkinstr opc 4 []     = Right <$> format4 opc True True False 0
    mkdirec "BYTE" [Operand (Left v) OpImmediate] = Right <$> byte v
    mkdirec "WORD" [Operand (Left v) OpSimple] = Right <$> word v
    mkdirec "RESB" [Operand (Left n) OpSimple] = Right <$> resb (fromIntegral n)
    mkdirec "RESW" [Operand (Left n) OpSimple] = Right <$> resw (fromIntegral n)
    mkdirec "START" [Operand (Left n) OpSimple] = Right <$> start (fromIntegral n)
    mkdirec "END" _ = return $ Right []
    mkdirec "BASE" _ = return $ Right []
    mkdirec a o = return $ Left $ ('\'':a) ++ "' is not a directive."

-- | Calculates the absolute address contained in an operand.
getAddr :: Operand -> Assembler (Result Address)
getAddr (Operand (Left s) _) = return $ Right $ fromIntegral s
getAddr (Operand (Right s) _) = getSymbol s
 
--
-- Helpers
--

-- | Determines if @operand@ is the index register.
isIndexingReg :: Operand -> Bool
isIndexingReg (Operand (Right v) OpSimple) = v == fst indexingRegister
isIndexingReg _ = False

-- | Determines if @operand@ should be absolute in format 3.
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

-- | Calculates the address displacement given
-- the start of the current instruction (@addr@)
-- and the offset to calc displacement for @memoff@
calcDisp :: Address -> Assembler (Maybe Int)
calcDisp memoff = do
  addr <- fromIntegral <$> address
  let adjust = -3
  if isPCRelative $ adjust + m - addr
    then return $ Just $ adjust + m - addr
    else do
      b <- maybe maxBound fromIntegral <$> getBase
      if isBaseRelative $ m - b
        then return $ Just $ m - b
        else return Nothing
  where m = fromIntegral memoff
        signbit = (finiteBitSize (undefined :: Int)) - 1

isPCRelative :: (Ord a, Num a) => a -> Bool
isPCRelative v = v >= -2048 && v < 2048

isBaseRelative :: (Ord a, Num a) => a -> Bool
isBaseRelative v = v >= 0 && v < 4096

lookupMnemonic :: String -> Result OpDesc
lookupMnemonic m = fromM ("invalid mnemonic: " ++ m) $ find ((==) m . opdescMnemonic) operations

lookupRegister :: String -> Result Word8
lookupRegister r = fromM ("register doesn't exist: " ++ r) $ lookup r registers

-- | Turns an operand into either a register code or its integral value.
format2Operand :: Operand -> Result Word8
format2Operand (Operand v OpSimple) = either (Right .fromIntegral) lookupRegister v
format2Operand (Operand v _) = Left $ "Operand '" ++ either show id v ++ "' is not compatible with format 2 instructions."

--
-- Second Pass (Low Level)
--

-- | Assembles a 24-bit word constant
word :: Integer -> Assembler [Word8]
word i = (packBits $ toWordN 24 w) <$ advanceAddress 3
  where w = ((fromIntegral i) :: Word32)

-- | Assembles a binary constant
byte :: Integer -> Assembler [Word8]
byte bs = do
  advanceAddress $ fromIntegral $ length bs'
  return bs'
  where bs' = integerToBytes bs

-- | Reserve bytes of space.
resb :: Word32 -> Assembler [Word8]
resb i = replicate (fromIntegral i) 0x0 <$ advanceAddress i

-- | Reserve words of space.
resw :: Word32 -> Assembler [Word8]
resw = resb . (* 3)

-- | Start directive.
start :: Word32 -> Assembler [Word8]
start = resb

-- | Assembles a Format 1 instruction.
format1 :: Word8 -> Assembler [Word8]
format1 w = [w] <$ advanceAddress 1

-- | Assembles a Format 2 instruction.
format2 :: Word8 -> Word8 -> Word8 -> Assembler [Word8]
format2 op a b = [op, shiftL a 4 .|. b] <$ advanceAddress 2

-- | Assembles a Format 3 instruction.
format3 :: Bool -> Word8 -> Bool -> Bool -> Bool -> Word32 -> Assembler [Word8]
format3 absolute op n i x memoff = do
  addr <- address
  
  disp <- fromMaybe 10000 <$> calcDisp memoff -- 10000 is longer than any valid relative offset * 2.
  let (b, p) = getBP addr memoff disp absolute 
  if b || p || absolute
    then do
      advanceAddress 3
      return $ getBytes b p addr $ fromIntegral disp
    else format4 op n i x memoff
  where
    getBytes b p addr boff = packBits $ (++) prefix $ toWordN 12 (disp :: Word32)
      where disp = bool boff memoff absolute
            prefix = format34DRY op n i x b p False
    getBP _ off _ True = (False, False)
    getBP addr boff off False = (p, b)
      where b = boff >= 0 && isBaseRelative boff
            p = isPCRelative boff

-- | Assembles a Format 4 instruction
format4 :: Word8 -> Bool -> Bool -> Bool -> Word32 -> Assembler [Word8]
format4 op n i x addr = packBits (prefix ++ toWordN 20 addr) <$ advanceAddress 4
  where prefix = format34DRY op n i x False False True

format34DRY :: Word8 -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> [Bool]
format34DRY op n i x b p e = toWordNEnd 6 op ++ [n, i, x, b, p, e]

-- | Turns a FiniteBits into a Big Endian WordN, truncating most -> least sig
toWordN :: FiniteBits a => Int -> a -> [Bool]
toWordN n = reverse . take n . toBits

-- | Turns a FiniteBits into a Big Endian WordN, truncating least -> most sig
toWordNEnd :: FiniteBits a => Int -> a -> [Bool]
toWordNEnd n = take n . reverse . toBits

-- | Turns a @FiniteBits a@ into a list of bits in bit endian order, with no special ordering of bits.
-- 'toBits' is dedicated to Fritz Wiedmer, my grandfather (~1925 to 2016). During
-- his time at IBM, he designed Bubbles memory and ECC for keyboards. Fritz is the
-- reason I became fascinated with computer science.
toBits :: FiniteBits a => a -> [Bool]
toBits x = map (testBit x) [0..finiteBitSize x - 1]

-- | Packs a list of bits into a big endian list of bytes
packBits :: [Bool] -> [Word8]
packBits = unfoldr f
  where
    f [] = Nothing
    f xs = Just (b, xs')
      where (bits, xs') = splitAt 8 xs
            indeces = [7,6,5,4,3,2,1,0]
            b = sum $ map (uncurry bit') $ zipWith (,) indeces $ fillTo 8 False bits
    bit' i True = bit i
    bit' i False = zeroBits
    fillTo n d xs = xs' ++ replicate (n - length xs') d
      where xs' = take n xs

