module Main (main) where

import           Interpreter
import           Lex
import           Optimizer
import           System.Environment (getArgs)
import           System.IO          (BufferMode (NoBuffering), hSetBuffering,
                                     stdout)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  [bfFilePath] <- getArgs
  bfSource <- readFile bfFilePath
  let lexes = lexBf bfSource

  let commands = case optimize lexes of
        Nothing   -> error "Error: Could not parse"
        Just cmds -> cmds

  print commands

  mta <- interpret commands
  -- TODO: Exit with a non-zero exit code
  case mta of
    Nothing -> putStrLn "Crashed"
    Just _  -> return ()
