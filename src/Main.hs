module Main (main) where

import Interpreter
import Lex
import Optimizer
import System.Environment (getArgs)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  inputs <- getContents

  [bfFilePath] <- getArgs
  bfSource <- readFile bfFilePath
  let lexes = lexBf bfSource

  let commands = case optimize lexes of
        Left err -> error err
        Right cmds -> cmds

  -- print commands

  let (RunResult _ _ (InOut _ out) st) = interpret inputs commands
  _ <- case st of
    RunFailure err -> error err
    RunSuccess -> return ()

  putStr out
