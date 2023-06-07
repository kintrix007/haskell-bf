module Lex (Token, charToLex, lexBf) where

import Data.Maybe (mapMaybe)

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

lexBf :: String -> [Token]
lexBf = mapMaybe charToLex
