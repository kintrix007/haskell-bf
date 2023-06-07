module Main (main) where

import Lex
import System.Environment (getArgs)
import Optimizer
import Parser

main :: IO ()
main = do
  [bfFilePath] <- getArgs
  bfSource <- readFile bfFilePath
  let lexes = lexBf bfSource
  let optimized = optimize lexes

  print bfSource
  print lexes
  print optimized
