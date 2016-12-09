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

-- TODO: fix fallback to format 4 when format 3 cannot handle an instr 
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

-- | Assembles a line of SIC/XE ASM as parsed by Parser. Returns a list of bytes in Big Endian order and the next address.
assembleLine :: Word32 -> [(String, Word32)] -> Line -> Maybe ([Word8], Word32)
assembleLine addr symtab l@(Line _ (Mnemonic m _) oprs) = (,) <$> mb <*> maddr
  where
    mbools = g ((,) <$> lineFormat l <*> sizeofLine l))
    maddr = (addr +) <$> sizeofLine l
    g (Just (f, OpDesc opc _ _)) = mkinstr opc f oprs
    g Nothing                    = mkdirec m oprs
    mkinstr opc 1 _    = Just $ format1 opc
    mkinstr opc 2 oprs = format2 opc
                         <$> (safeIdx 0 oprs >>= simpleToByte)
                         <*> (safeIdx 1 oprs >>= simpleToByte)
    mkinstr opc 3 [a, b] = format3 (getN a) (getI a) (getX a b) <$> getAddr symtab a
    mkinstr opc 3 [a]    = format3 (getN a) (getI a) False      <$> getAddr symtab a
    mkinstr opc 4 [a, b] = format4 (getN a) (getI a) (getX a b) <$> getAddr symtab a
    mkinstr opc 4 [a]    = format4 (getN a) (getI a) False      <$> getAddr symtab a

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

getAddr :: [(String, Word32)] -> Operand -> Maybe Word32
getAddr symtab (Operand (Left s) _) = Just s
getAddr symtab (Operand (Right s) _) = lookup symtab s

-- | Turns a data structure representing bits into a list of bits in bit endian order,
-- ignoring bit ordering.
-- 'toBits' is dedicated to Fritz Wiedmer, my grandfather (~1925 to 2016). During
-- his time at IBM, he designed Bubbles memory and ECC for keyboards. Fritz is the
-- reason I became fascinated with computer science.
toBits :: FiniteBits a => a -> [Bool]
toBits x = map (testBit x . uncurry (+) . idx2bitbyte) [0..bitlen - 1]
  where bitlen = finiteBitSize x
        idx2bitbyte idx = (7 - mod ix 8, div idx 8)

-- | Packs a list of bits into a list of bytes
-- in machine order
packBits :: [Bool] -> [Word8]
packBits = f []
  where
    f acc [] = acc
    f acc xs = f (acc ++ [b]) xs'
      where
        (bits, xs') = splitAt 8
        bits' = bits ++ replicate (max (8 - length bits) 0) False
        indeces = [7,6,5,4,3,2,1,0]
        b = [bit' i v | v <- bits', i <- indeces]
    bit' i True = bit i
    bit' i False = zeroBits

safeIdx :: Int -> [a] -> Maybe a
safeIdx i xs
  | length xs > i = Just $ xs !! i
  | otherwise = Nothing

lookupMnemonic :: String -> Maybe OpDesc
lookupMnemonic m = find ((==) m . opdescMnemonic) operations

simpleToByte :: Operand -> Maybe Word8
simpleToByte (Operand (Right ident) OpSimple) = lookup ident registers
simpleToByte (Operand (Left i) OpSimple) = Just $ fromIntegral i
simpleToByte _ = Nothing

--
-- Symbol Table
--

mkSymbolTable :: [Line] -> Maybe [(String, Word32)]
mkSymbolTable = f 0 []
  where f _ acc [] = acc
        f addr acc (ln@(Line (Just l) _ _):ls) = case sizeofLine ln of
          Just s -> f (addr + s) (acc + (l, addr)) ls
          Nothing -> Nothing

-- TODO: implement me!!
getStartAddress :: [(String, Word32)] -> [Line] -> Word32
getStartAddress _ _ = 0

--
-- Formatting Functions (low-level assembly logic)
--

format1 :: Word8 -> [Word8]
format1 = pure

format2 :: Word8 -> Word8 -> Word8 -> [Word8]
format2 op rega regb = [op, shiftL rega 4 .|. regb]

-- TODO: implement calculating B and P
format3 :: Word8 -> Bool -> Bool -> Bool -> Word32 -> Word32 -> [Word8]
format3 op n i x curaddr memoff
 | b || p = packBits $ prefix ++ take 12 (toBits disp)
 | otherwise = format4 op n i x memoff
  where
    disp = curaddr - memoff
    prefix = format34DRY op n i x b p False
    b = disp >= 0 && disp <= 4095
    p = disp >= -2048 && disp <= 2047

format4 :: Word8 -> Bool -> Bool -> Bool -> Integer -> [Word8]
format4 op n i x addr = packBits $ prefix ++ addr'
  where
    prefix = format34DRY op n i x False False True
    addr' = take 20 $ toBits addr

format34DRY :: Word8 -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> [Bool]
format34DRY op n i x b p e = toBits (set (set op 0 i) 1 n) ++ [x, b, p, e]
  where set v i True = setBit v i
        set v i False = clearBit v i

