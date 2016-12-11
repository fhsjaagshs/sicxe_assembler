module Main where

import Common
import Parser
import Assembler

import Data.Maybe
import System.Environment
import System.IO

main :: IO ()
main = do
  inname <- fromMaybe "in.sic" . safeIdx 0 <$> getArgs
  outname <- fromMaybe "out.exe" . safeIdx 1 <$> getArgs
  srclines <- splitLines <$> readFile inname
  let lines = mapM parseLine $ map tokenizeLine srclines
  maybe (failure inname) (success outname srclines) $ assemble =<< lines
  where
    failure n = putStrLn $ "failed to assemble " ++ n
    success outname src asm = do
      withBinaryFile outname WriteMode ((flip hPutBytes) (mconcat asm))
      printer 0 src asm
    printer addr (s:src) (a:asm) = do
      putStrLn $ show addr ++ s ++ "\t" ++ showHex a
      printer (addr + (length a)) src asm 
    printer _ _ _ = return ()
    splitLines = filter (not . c) . splitOn ['\n']
      where c ('.':_) = True
            c _ = False


