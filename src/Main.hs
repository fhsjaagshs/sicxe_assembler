module Main where

import Common
import Buffer
import Parser
import System.Environment
import Foreign.Marshal.Array

main :: IO ()
main = do
  inname <- fromMaybe "in.sic" . safeIdx 0 <$> getArgs
  outname <- fromMaybe "out.exe" . safeIdx 1 <$> getArgs
  srclines <- splitLines <$> readFile inname
  let lines = mapM parseLine =<< mapM tokenizeLine srclines
  maybe (failure inname) (success outname srclines) $ assemble lines
  where
    failure = putStrLn . (++) "failed to assemble "
    success outname src asm = do
      withFile outname $ flip hPutBytes (mconcat asm)
      printer 0 src asm
    printer addr (s:src) (a:asm) = do
      putStrLn $ show addr ++ s ++ "\t" ++ showHex a
      printer (addr + (length a)) src asm 
    printer _ _ _ = return ()
    splitLines = filter (not . c) . splitOn ['\n']
      where c ('.':_) = True
            c _ = False

showHex :: [Word8] -> String
showHex = mconcat . map toPair
  where
    toPair w = [f h, f l] 
      where
        f x
          | x < 10 = chr $ ord '0' + x
          | otherwise = chr $ ord 'A' + x
        l = w .&. 0x0F
        h = shiftR (w .&. 0xF0) 4
  

hPutBytes :: Handle -> [Word8] -> IO ()
hPutBytes hdl bytes = withArray bytes (hPutBuf hdl)

