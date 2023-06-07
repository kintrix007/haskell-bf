module Main (main) where

import Data.Maybe (mapMaybe)
import System.Environment (getArgs)

data Token
  = Plus
  | Minus
  | StepL
  | StepR
  | Open
  | Close
  | Read
  | Write
  deriving (Eq, Show, Enum)

charToLex :: Char -> Maybe Token
charToLex '+' = Just Plus
charToLex '-' = Just Minus
charToLex '<' = Just StepL
charToLex '>' = Just StepR
charToLex '[' = Just Open
charToLex ']' = Just Close
charToLex ',' = Just Read
charToLex '.' = Just Write
charToLex _ = Nothing

main :: IO ()
main = do
  [bfFilePath] <- getArgs
  bfSource <- readFile bfFilePath
  let tokens = mapMaybe charToLex bfSource

  -- I am in pain. It is so late I can barely code.

  print bfSource
  print tokens
