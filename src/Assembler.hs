{-|

-}

{-# #-}

module Assembler
(
  sizeofLine,
  assembleLine,
  mkSymbolTable
)
where

import Parser
import Definitions
import Data.Word
import Data.Bits

--
-- High level assembly logic
--

lineFormat :: Line -> Maybe Int 
lineFormat (Line _ (Mnemonic _ extended) oprs) = case lookupMnemonic m of
  Nothing -> Nothing
  Just (OpDesc _ _ [1]) = Just 1
  Just (OpDesc _ _ [2]) = Just 2
  Just (OpDesc _ _ fs) = case (extended, 3 `elem` fs, 4 `elem` fs) of
    (False, True, False) -> Just 3
    (True, False, True) -> Just 4
    _ -> Nothing

-- TODO: implement sizeofLine for directives
sizeofLine :: Line -> Maybe Int
sizeofLine = lineFormat

-- | Assembles a line of SIC/XE ASM as parsed by Parser. Returns a buffer and the next address.
assembleLine :: Word32 -> [(String, Word32)] -> Line -> Maybe ([Bool], Word32)
assembleLine addr symtab l@(Line _ (Mnemonic m _) oprs) = (,) <$> mb <*> maddr
  where
    mbools = g ((,) <$> lineFormat l <*> sizeofLine l))
    maddr = (addr +) <$> sizeofLine l
    g (Just (f, OpDesc opc _ _)) = mkinstr opc f oprs
    g Nothing                    = mkdirec m oprs
    mkinstr opc 1 _    = Just $ format1 opc
    mkinstr opc 2 oprs = format2 opc
                         <$> (safeIdx 0 oprs >>= lookupRegister)
                         <*> (safeIdx 1 oprs >>= lookupRegister)
    mkinstr opc 3 [a, b] = format3 (getN a) (getI a) (getX a b) <$> getDisp addr symtab a
    mkinstr opc 3 [a]    = format3 (getN a) (getI a) False      <$> getDisp addr symtab a
    mkinstr opc 4 [a, b] = format4 (getN a) (getI a) (getX a b) <$> getAddr a
    mkinstr opc 4 [a]    = format4 (getN a) (getI a) False      <$> getAddr a

    -- TODO: IMPLEMENT DIRECTIVES
    --       1. ones that assemble
    --       2. ones that don't assemble
    mkdirec str oprs = return Nothing
    
--
-- Helpers
--

isIndexingReg :: Operand -> Bool
isIndexingReg (Operand (Right v) OpSimple) = v == (fst $ indexingRegister)
isIndexingReg _ = False

isType :: OperandType -> Operand -> Bool
isType t2 (Operand _ t) = t == t2

getX :: Operand -> Operand -> Bool
getX a b = isIndexingReg b && isType OpSimple a

getI :: Operand -> Bool
getI a = isType OpImmediate a || isType OpSimple a

getN :: Operand -> Bool
getN a = isType OpIndirect a || isType OpSimple a

getDisp :: Word32 -> [(String, Word32)] -> Operand -> Maybe Int
getDisp addr symtab = const Nothing

getAddr :: Operand >- Maybe Int
getAddr = const Nothing

-- | Turns a data structure representing bits into a list of bits in bit endian order,
-- ignoring bit ordering.
-- 'toBits' is dedicated to Fritz Wiedmer, my grandfather (~1925 to 2016). During
-- his time at IBM, he designed Bubbles memory and ECC for keyboards. Fritz is the
-- reason I became fascinated with computer science.
toBits :: FiniteBits a => a -> [Bool]
toBits x = map (testBit x . uncurry (+) . idx2bitbyte) [0..bitlen - 1]
  where bitlen = finiteBitSize x
        idx2bitbyte idx = (7 - mod ix 8, div idx 8)

safeIdx :: Int -> [a] -> Maybe a
safeIdx i xs
  | length xs > i = Just $ xs !! i
  | otherwise = Nothing

lookupMnemonic :: String -> Maybe OpDesc
lookupMnemonic m = find ((==) m . opdescMnemonic) operations

lookupRegister :: Operand -> Maybe Word8
lookupRegister (Operand (Right ident) OpSimple) = lookup ident registers
lookupRegister (Operand (Left i) OpSimple) = Just $ fromIntegral i
lookupRegister _ = Nothing

--
-- Symbol Table
--

mkSymbolTable :: [Line] -> Maybe [(String, Word32)]
mkSymbolTable = f 0 []
  where f _ acc [] = acc
        f addr acc (ln@(Line (Just l) _ _):ls) = case sizeofLine ln of
          Just s -> f (addr + s) (acc + (l, addr)) ls
          Nothing -> Nothing

--
-- Formatting Functions (low-level assembly logic)
--

format1 :: Word8 -> [Bool]
format1 = toBits

format2 :: Word8 -> Word8 -> Word8 -> [Bool]
format2 op rega regb = toBits op ++ toBits regb
  where regs = ((rega `shiftL` 4) .|. regb)

format3 :: Word8 -> Bool -> Bool -> Bool -> Bool -> Bool -> Integer -> [Bool]
format3 op n i x b p disp = format32DRY op n i x b p ++ (take 12 $ toBits disp) -- Is this going to be the right order?

format4 :: Word8 -> Bool -> Bool -> Bool -> Integer -> [Bool]
format4 op n i x addr = format34DRY op n i x False False True ++ (take 20 $ toBits addr)

format34DRY :: Word8 -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> [Bool]
format34DRY op n i x b p e = toBits (set (set op 0 i) 1 n) ++ [x, b, p, e]
