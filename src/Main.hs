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
  let lines = map tokenizeLine lines
      syntax = mapM parseLine lines
  buffers <- mapM assembleLine syntax
  print syntax
  return []
  where
    printlns [] _ = return ()
    printlns _ [] = return ()
    printlns (l:ls) (b:bs) = do
      bhex <- showBufferHex b
      putStrLn $ intercalate "\t" $ l ++ [bhex]
      printlns ls bs

buildSymbolTable :: 

