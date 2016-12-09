{-|

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
import Definitions
import Data.Word
import Data.Bits

--
-- Helpers
--

safeIdx :: Int -> [a] -> Maybe a
safeIdx i xs
  | length xs > i = Just $ xs !! i
  | otherwise = Nothing

lookupMnemonic :: String -> Maybe OpDesc
lookupMnemonic m = find ((==) m . opdescMnemonic) operations

lookupRegister :: Operand -> Maybe Word8
lookupRegister (ImmOperand _) = Nothing
lookupRegister (ConstOperand c) = Just $ fromIntegral c -- For things like SHIFTR and SHIFTL
lookupRegister (IdOperand ident) = lookup ident registers

--
-- High level assembly logic

sizeofLine :: Line -> Int 

-- TODO: implement assembly
assembleLine :: Line -> IO (Maybe Buffer)
assembleLine (Line _ (Mnemonic m extended) oprs) = do
  case lookupMnemonic of
    Nothing -> return Nothing -- TODO: directives would go here
    Just (OpDesc opc _ fs) -> do
      if 1 `elem` fs
        then format1 opc
        else if 2 `elem` fs
          then format2 op
                 <$> (safeIdx 0 oprs >>= lookupRegister)
                 <$> (safeIdx 1 oprs >>= lookupRegister)
          else return Nothing -- NO IDEA HOW TO IMPLEMENT 3/4 

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

format3 :: Word8 -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Word16 -> IO (Maybe Buffer)
format3 op n i x b p e disp = do
  b <- newBuffer 24
  success <- format34DRY b op n i x b p e
  if success
    then do
      dispb <- fromBitlike disp
      bufcpy (b `plusBuf` 11) dispb 12
      return $ Just b
    else do
      freeBuffer b
      return Nothing

format4 :: Word8 -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Word32 -> IO (Maybe Buffer)
format4 op n i x b p e addr = do
  b <- newBuffer 32
  success <- format34DRY b op n i x b p e
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

