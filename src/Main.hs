module Main (main) where

import Interpreter
import Lex
import Optimizer
import System.Environment (getArgs)

main :: IO ()
main = do
  inputs <- getContents

  [bfFilePath] <- getArgs
  bfSource <- readFile bfFilePath
  let lexes = lexBf bfSource

  let commands = case optimize lexes of
        Left err -> error err
        Right cmds -> cmds

  let output = interpret inputs commands

  putStr output
