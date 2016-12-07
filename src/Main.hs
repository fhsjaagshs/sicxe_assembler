module Main where

import Buffer
import Parser
import System.Environment

main :: IO ()
main = do
  inname <- safeHead <$> getArgs
  outname = (=<<) safeHead . safeTail <$> getArgs
  out <- openFile outname
  lines <- parseFile <$> readFile inname
  print lines
  -- buffers <- mapM assembleLine lines
  -- mapM (hPutBuffer out) buffers
  -- mapM freeBuffer buffers
  where
    parseFile = (=<<) mapM parseLine . mapM tokenizeLine . splitLines
    safeHead [] = Nothing
    safeHead (x:_) = Just x
    safeTail [] = Nothing
    safeTail (_:xs) = Just xs
    splitLines = unfoldr f
    f "" = Nothing
    f xs = (fmap splitAt $ elemIndex '\n' xs') <*> stripNL xs
      where stripNL [] = Nothing
            stripNL ('\n':xs) = stripNL xs 
            stripNL xs = Just xs

-- Assembles each line to a buffer
assemble :: [String] -> IO [Buffer]
assemble lines = do
  let syntax = tokenizeLine lines >>= parseLine
      
  print syntax
  return []

buildSymbolTable :: 

