module Main (main) where

import Lex
import System.Environment (getArgs)

main :: IO ()
main = do
  [bfFilePath] <- getArgs
  bfSource <- readFile bfFilePath
  let lexes = lexBf bfSource

  print bfSource
  print lexes
