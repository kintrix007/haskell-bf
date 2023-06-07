module Main (main) where

import Lex
import System.Environment (getArgs)
import Optimizer

main :: IO ()
main = do
  [bfFilePath] <- getArgs
  bfSource <- readFile bfFilePath
  let lexes = lexBf bfSource
  let optimized = optimize lexes

  -- print bfSource
  -- putStrLn "\n\n"
  -- print lexes
  -- putStrLn "\n\n"
  print optimized
