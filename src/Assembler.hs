{-|

-}

{-# #-}

module Assembler
(
  lineFormat,
  sizeofLine,
  assembleLine
)
where

import Parser
import Buffer
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

assembleLine :: Word32 -> [(String, Word32)] -> Line -> IO (Maybe Buffer)
assembleLine addr symtab l@(Line _ (Mnemonic m _) oprs) =
 g ((,) <$> lineFormat l <*> lookupMnemonic m)
  where
    g (Just (f, OpDesc opc _ _)) = mkinstr opc f oprs
    g Nothing                    = mkdirec m oprs
    mkinstr opc 1 _    = format1 opc
    mkinstr opc 2 oprs = format2 opc
                         <$> (safeIdx 0 oprs >>= lookupRegister)
                         <*> (safeIdx 1 oprs >>= lookupRegister)
    mkinstr opc 3 [a, b] = getDisp addr symtab a >>= format3 (getN a) (getI a) x b p
      where x = isIndexingReg b && isType OpSimple a
    mkinstr opc 3 [a] = getDisp addr symtab a >>= format3 (getN a) (getI a) False b p
    mkinstr opc 4 [a, b] = getAddr a >>= format4 (getN a) (getI a) x b p
      where x = isIndexingReg b && isType OpSimple a
    mkinstr opc 4 [a] = getAddr a >>= format4 (getN a) (getI a) False b p

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

getI :: Operand -> Bool
getI a = isType OpImmediate a || isType OpSimple a

getN :: Operand -> Bool
getN a = isType OpIndirect a || isType OpSimple a

getDisp :: Word32 -> [(String, Word32)] -> Operand -> Maybe Int
getDisp addr symtab = const Nothing

getAddr :: Operand >- Maybe Int
getAddr = const Nothing

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
-- Formatting Functions (low-level assembly logic)
--  * by the point these are used, all semantic errors should have
--  been discovered. These may fail for low-level layout errors.
--

format1 :: Word8 -> IO (Maybe Buffer)
format1 = fmap Just . fromBitlike

format2 :: Word8 -> Word8 -> Word8 -> IO (Maybe Buffer)
format2 op rega regb
  | otherwise = do
    buf <- newBuffer 16
    setByte 0 op
    setByte 1 ((rega `shiftL` 4) .|. regb)
    return $ Just buf

-- TODO: find out which combinations of arguments are forbidden

format3 :: Word8 -> Bool -> Bool -> Bool -> Bool -> Bool -> Word16 -> IO (Maybe Buffer)
format3 op n i x b p disp = do
  b <- newBuffer 24
  success <- format34DRY b op n i x b p False
  if success
    then do
      dispb <- fromBitlike disp
      bufcpy (b `plusBuf` 11) dispb 12
      return $ Just b
    else do
      freeBuffer b
      return Nothing

format4 :: Word8 -> Bool -> Bool -> Bool -> Word32 -> IO (Maybe Buffer)
format4 op n i x addr = do
  b <- newBuffer 32
  success <- format34DRY b op n i x False False True
  if success
    then do
      addrb <- fromBitlike addr
      bufcpy (b `plusBuf` 11) 20
      return $ Just b
    else do
      freeBuffer b
      return Nothing

format34DRY :: Buffer -> Word8 -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> IO Bool
format34DRY b op n i x b p e = do
  setByte b 0 op
  setBit  b 6 n
  setBit  b 7 i
  setBit  b 8 x
  setBit  b 9 b
  setBit  b 10 p
  setBit  b 11 e
  return True 

