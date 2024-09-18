{-# LANGUAGE TupleSections #-}

module Lex
  ( Lex (..)
  , charToLex
  , lexBf
  ) where

import           Data.Maybe (mapMaybe, maybeToList)

data Lex
  = LPlus
  | LMinus
  | LLeft
  | LRight
  | LOpen
  | LClose
  | LRead
  | LWrite
  deriving (Eq, Enum)

instance Show Lex where
  show LPlus  = "+"
  show LMinus = "-"
  show LLeft  = "<"
  show LRight = ">"
  show LOpen  = "["
  show LClose = "]"
  show LRead  = ","
  show LWrite = "."

instance Read Lex where
  readsPrec _ []     = []
  readsPrec _ (c:cs) = map (,cs) $ maybeToList $ charToLex c

charToLex :: Char -> Maybe Lex
charToLex '+' = Just LPlus
charToLex '-' = Just LMinus
charToLex '<' = Just LLeft
charToLex '>' = Just LRight
charToLex '[' = Just LOpen
charToLex ']' = Just LClose
charToLex ',' = Just LRead
charToLex '.' = Just LWrite
charToLex _   = Nothing

lexBf :: String -> [Lex]
lexBf = mapMaybe charToLex
