{-|
Module: Assembler
Description: Assemble data structures from Parser into object code.
-}

module Assembler
(
  assemble
)
where

import Common
import Parser
import Definitions
import Data.Word
import Data.Bits
import Data.List

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

import Data.Maybe
import Control.Applicative
import Control.Monad.State.Strict

assemble :: [Line] -> Result [[Word8]]
assemble = (=<<) f . mapM preprocessLine
  where
    f ls = runAssembler 0 mempty $ do
      firstPass ls
      setAddress 0
      secondPass ls

type Address = Word32 -- | 1MB Address
type SymbolTable = HashMap String Address -- | Symbol table
type Assembler a = State (Address, SymbolTable, Either String (Maybe Address)) a -- | Assembler monad

-- | Run an assembler monad.
runAssembler :: Address -> SymbolTable -> Assembler a -> a
runAssembler a st ass = evalState ass (a, st, Right Nothing)

-- | Get the current address.
address :: Assembler Address
address = fst' <$> get

-- | Set the current address 
setAddress :: Address -> Assembler ()
setAddress addr = state $ \(_, st, b) -> ((), (addr, st, b))

-- | Gets the base address.
getBase :: Assembler (Maybe Address)
getBase = f . thd' =<< get
  where f (Left str) = fmap toM $ getSymbol str
        f (Right a) = return a

-- | Set the base address
setBase :: Address -> Assembler ()
setBase a = state $ \(addr, st, _) -> ((), (addr, st, Right $ Just a))

-- | Set the base address to a value from the parser.
-- If it's an integer, it immediately sets the base.
-- Otherwise, it stores an identifier to look up later.
-- This is so that the base directive can reference
-- labels farther down in the source code.
setBaseValue :: Either Integer String -> Assembler ()
setBaseValue (Left i) = setBase $ fromIntegral i
setBaseValue (Right v) = state $ \(addr, st, _) -> ((), (addr, st, Left v))

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
-- Preprocessing
--

-- | Preprocesses line using Definitions.hs:
-- 1. Validates extended attribute
-- 2. Validates number of operands, truncating if necessary
-- 3. Validates each operand after truncation
-- 4. Applies transforms to each operand.
preprocessLine :: Line -> Result Line
preprocessLine l@(Line lbl (Mnemonic m ext) oprs)
  | m `elem` ["BYTE", "WORD", "RESB", "RESW", "START", "END", "BASE"] = Right l
  | otherwise = lookupMnemonic m >>= f
  where
    f (OpDesc _ _ fs no xform vs)
      | not hasEnoughOperands = Left "not enough operands"
      | not (elem 4 fs) && ext = Left "non extensible mnemonic extended"
      | not operandsMatch = Left "invalid operands"
      | otherwise = Right $ Line lbl (Mnemonic m ext) oprs''
      where
        validator = fromMaybe (\_ _ -> True) $ lineFormat l >>= flip HM.lookup vs
        hasEnoughOperands = (length oprs) >= no
        operandsMatch = all (uncurry validator) $ zipWith (,) oprs [0..no - 1] 
        oprs' = take no oprs
        oprs''
          | fromMaybe False ((< no) <$> idxRegIdx) = map xform oprs'
          | otherwise = map xform $ oprs' ++ (maybe [] pure $ find isIndexingReg oprs)
          where
            idxRegIdx = findIndex isIndexingReg oprs

--
-- First Pass
--

-- | Reads directives & labels, preparing the assembler
-- staste to assemble SIC/XE code. Updates symtab & base.
firstPass :: [Line] -> Assembler ()
firstPass [] = return ()
firstPass ((Line _ (Mnemonic "BASE" False) [Operand v OpSimple]):ls) = do
  setBaseValue v
  firstPass ls
firstPass (l@(Line mlbl _ _):ls) = do
  maybe (return ()) (act mlbl) $ sizeofLine l
  firstPass ls
  where
    act (Just lbl) size = do
      setSymbol lbl =<< address
      advanceAddress size
    act Nothing size = advanceAddress size

--
-- Second Pass (High Level)
-- This is the part that handles parser data structures
--

-- | Assembles each line into object code.
secondPass :: [Line] -> Assembler (Result [[Word8]])
secondPass = fmap sequence . mapM assembleLine

-- | Determine the format of a line of SIC/XE code.
lineFormat :: Line -> Maybe Int
lineFormat (Line _ (Mnemonic m ext) oprs) = find (valid oprs) . opdescFormats =<< toM (lookupMnemonic m)
  where
    valid []     1 = True
    valid (x:xs) 2 = and $ map (isType OpSimple) (x:xs)
    valid []     3 = True
    valid (x:_)  3 | ext = False | reqAbs x = True | otherwise = True
    valid _      4 = True
    valid _      _ = False

-- | Determine the assembled size of a line of SIC/XE code.
sizeofLine :: Line -> Maybe Word32
sizeofLine l@(Line _ (Mnemonic m _) oprs) = (fromIntegral <$> lineFormat l) <|>  ds m oprs
  where
    ds "BYTE" [Operand (Left v) OpImmediate] = Just $ fromIntegral $ length $ integerToBytes v
    ds "WORD" [Operand (Left _) OpSimple] = Just 3
    ds "RESB" [Operand (Left n) OpSimple] = Just $ fromIntegral n
    ds "RESW" [Operand (Left n) OpSimple] = Just $ 3 * (fromIntegral n)
    ds "START" [Operand (Left n) OpSimple] = Just $ fromIntegral n
    ds "END" _ = Just 0
    ds "BASE" _ = Just 0
    ds _ _ = Nothing

-- | Assembles a line of SIC/XE code as parsed by Parser.
-- Returns a list of bytes in Big Endian order.
assembleLine :: Line -> Assembler (Result [Word8])
assembleLine l@(Line _ (Mnemonic m _) oprs) = g $ (,) <$> lf <*> lookupMnemonic m
  where
    lf = fromM "invalid line" $ lineFormat l
    g (Right (f, o)) = mkinstr (opdescOpcode o) f oprs
    g (Left _)       = mkdirec m oprs
    mkinstr opc 1 _      = Right <$> format1 opc
    mkinstr opc 2 [a]    = applyResultA2 (format2 opc) (format2Operand a) (Right 0)
    mkinstr opc 2 [a, b] = applyResultA2 (format2 opc) (format2Operand a) (format2Operand b)
    mkinstr opc 3 [a, b] = either (return . Left) (format3 (reqAbs a) opc (getN a) (getI a) (getX a b)) =<< getAddr a
    mkinstr opc 3 [a]    = either (return . Left) (format3 (reqAbs a) opc (getN a) (getI a) False) =<< getAddr a
    mkinstr opc 3 []     = format3 True opc True True False 0
    mkinstr opc 4 [a, b] = bindResultM (format4 opc (getN a) (getI a) (getX a b)) $ getAddr a
    mkinstr opc 4 [a]    = bindResultM (format4 opc (getN a) (getI a) False) $ getAddr a
    mkinstr opc 4 []     = Right <$> format4 opc True True False 0
    mkinstr _   _ _      = return $ Left "invalid instruction"
    mkdirec "BYTE" [Operand (Left v) OpImmediate] = Right <$> byte v
    mkdirec "WORD" [Operand (Left v) OpSimple] = Right <$> word v
    mkdirec "RESB" [Operand (Left n) OpSimple] = Right <$> resb (fromIntegral n)
    mkdirec "RESW" [Operand (Left n) OpSimple] = Right <$> resw (fromIntegral n)
    mkdirec "START" [Operand (Left n) OpSimple] = Right <$> start (fromIntegral n)
    mkdirec "END" _ = return $ Right []
    mkdirec "BASE" _ = return $ Right []
    mkdirec a _ = return $ Left $ ('\'':a) ++ "' is not a directive."

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

-- | Looks up a mnemonic in Definitions.hs
lookupMnemonic :: String -> Result OpDesc
lookupMnemonic m = fromM ("invalid mnemonic: " ++ m) $ find ((==) m . opdescMnemonic) operations

-- | Looks up a register in Definitions.hs
lookupRegister :: String -> Result Word8
lookupRegister r = fromM ("register doesn't exist: " ++ r) $ lookup r registers

-- | Formats operand to be used in a format 2 instruction.
format2Operand :: Operand -> Result Word8
format2Operand (Operand v OpSimple) = either (Right .fromIntegral) lookupRegister v
format2Operand (Operand v _) = Left $ "Operand '" ++ either show id v ++ "' is not compatible with format 2 instructions."

--
-- Second Pass (Low Level)
-- Generate object code!!!
--

-- | Calculates the address displacement from @memoff@ from either
-- PC or B, returning if the displacement is Base-Relative as well
-- as the displacement itself.
calcDisp :: Address -> Assembler (Result (Bool, [Bool]))
calcDisp m = do
  pc <- (+ 3) <$> address
  mb <- getBase
  if isPCRelative $ m `minus` pc
    then return $ return $ (False, toWordNS 12 $ m `minus` pc)
    else case mb of
      Nothing -> return $ Left $ "using base relative addressing in the absense of a base (PC = " ++ show pc ++ ")"
      Just b -> if isBaseRelative $ m `minus` b
                  then return $ return $ (True, toWordN 12 $ m `minus` b)
                  else return $ Left ("offset (" ++ show m ++ ") incompatible with PC- or B- relative addressing")
  where
    minus a b
      | a >= b = a - b
      | otherwise = (complement (b - a)) + 1
    isPCRelative v 
      | testBit v sidx = isPCRelative $ 1 + complement v
      | otherwise = v <= (0xFFF `shiftR` 1)
      where sidx = (finiteBitSize v) - 1
    isBaseRelative v = v >= 0 && v <= 0xFFF

    toWordNS n w = msb:toWordN (n - 1) w
      where msb = testBit w $ (finiteBitSize w) - 1

-- | Assembles a 24-bit word constant
word :: Integer -> Assembler [Word8]
word i = (packBits $ toWordN 24 w) <$ advanceAddress 3
  where w = ((fromIntegral i) :: Word32)

-- | Assembles a binary constant
byte :: Integer -> Assembler [Word8]
byte bs = bs' <$ (advanceAddress $ fromIntegral $ length bs')
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
format3 :: Bool -> Word8 -> Bool -> Bool -> Bool -> Word32 -> Assembler (Result [Word8])
format3 True  op n i x memoff = (Right $ packBits $ prefix ++ toWordN 12 memoff) <$ advanceAddress 3
  where prefix = toWordNEnd 6 op ++ [n, i, x, False, False, False]
format3 False op n i x memoff = f =<< calcDisp memoff
  where
    f (Left e) = return $ Left e
    f (Right (b, disp)) = Right (packBits (prefix ++ disp)) <$ advanceAddress 3
      where prefix = toWordNEnd 6 op ++ [n, i, x, b, not b, False]

-- | Assembles a Format 4 instruction
format4 :: Word8 -> Bool -> Bool -> Bool -> Word32 -> Assembler [Word8]
format4 op n i x addr = packBits (prefix ++ toWordN 20 addr) <$ advanceAddress 4
  where prefix = toWordNEnd 6 op ++ [n, i, x, False, False, True]

-- | Turns a FiniteBits into a Big Endian WordN, truncating keeping less sig bits
toWordN :: FiniteBits a => Int -> a -> [Bool]
toWordN n = reverse . take n . toBits

-- | Turns a FiniteBits into a Big Endian WordN, truncating keeping most sig bits
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
    bit' _ False = zeroBits
    fillTo n d xs = xs' ++ replicate (n - length xs') d
      where xs' = take n xs
