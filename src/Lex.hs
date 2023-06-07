module Lex (Lex (..), charToLex, lexBf) where

import Data.Maybe (mapMaybe)

data Lex
  = LPlus
  | LMinus
  | LLeft
  | LRight
  | LOpen
  | LClose
  | LRead
  | LWrite
  deriving (Eq, Show, Enum)

charToLex :: Char -> Maybe Lex
charToLex '+' = Just LPlus
charToLex '-' = Just LMinus
charToLex '<' = Just LLeft
charToLex '>' = Just LRight
charToLex '[' = Just LOpen
charToLex ']' = Just LClose
charToLex ',' = Just LRead
charToLex '.' = Just LWrite
charToLex _ = Nothing

lexBf :: String -> [Lex]
lexBf = mapMaybe charToLex
