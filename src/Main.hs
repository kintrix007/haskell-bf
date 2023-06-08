module Main (main) where

import Interpreter
import Lex
import Optimizer
import System.Environment (getArgs)
import Data.Array (elems)

main :: IO ()
main = do
  inputs <- getContents

  [bfFilePath] <- getArgs
  bfSource <- readFile bfFilePath
  let lexes = lexBf bfSource

  let commands = case optimize lexes of
        Left err -> error err
        Right cmds -> cmds

  print commands

  let (output, ar) = case interpret inputs commands of
        Nothing -> error "Something went wrong running the bf code"
        Just x -> x

  -- print $ reverse . dropWhile (==0) . reverse $ elems ar

  putStr output
