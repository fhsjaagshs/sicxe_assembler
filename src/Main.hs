module Main where

import Common
import Parser
import Assembler

import Data.Maybe
import System.Environment
import System.IO

main :: IO ()
main = do
  inname <- fromMaybe "main.asm" . safeIdx 0 <$> getArgs
  srclines <- splitLines <$> readFile inname
  let lines = mapM parseLine $ map tokenizeLine $ filter (not . isComment) srclines
      outname = takeWhile (/= '.') inname ++ ".exe"
  maybe (failure inname) (success outname srclines) $ assemble =<< lines
  where
    failure n = putStrLn $ "failed to assemble " ++ n
    success outname src asm = do
      withBinaryFile outname WriteMode ((flip hPutBytes) (mconcat asm))
      printer 0 src asm
    printer addr (s:src) (a:asm) = do
      putStrLn $ show addr ++ "\t" ++ s ++ "\t" ++ showHex a
      printer (addr + (length a)) src asm 
    printer _ _ _ = return ()
    splitLines = filter (not . c) . splitOn ['\n']
      where c ('.':_) = True
            c _ = False


