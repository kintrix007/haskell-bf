module Optimizer (Command (..), optimize, optimizeStep1) where

import Control.Applicative
import Data.Int (Int8)
import Lex
import Parser

data Command
  = Add !Int
  | Shift !Int
  | Move !Int
  | Move2 !Int !Int
  | Copy !Int
  | Zero
  | ScanRFor !Int8
  | ScanLFor !Int8
  | Loop ![Command]
  | Read
  | Write
  deriving (Show, Eq)

optimize :: [Lex] -> Maybe [Command]
optimize lexs = do
  (cmds, lexs') <- parse s1 lexs
  if not (null lexs')
    then Nothing
    else Just cmds
  where
    s1 = optimizeStep1

optimizeStep1 :: Parser Lex [Command]
optimizeStep1 = do
  ended <- streamEnded
  if ended
    then return []
    else liftA2 (++) parseStep1 optimizeStep1

parseLoop :: Parser Lex [Command]
parseLoop = do
  _ <- one LOpen
  cmds <- concat <$> many parseStep1
  _ <- one LClose
  return [Loop cmds]

parseAdd :: Parser Lex [Command]
parseAdd = do
  xs <- some (one LPlus <|> one LMinus)
  let total = sum . map value $ xs
  return [Add total]
  where
    value LPlus = 1
    value LMinus = -1
    value _ = error "This should never happen"

parseShift :: Parser Lex [Command]
parseShift = do
  xs <- some (one LLeft <|> one LRight)
  let total = sum . map value $ xs
  return [Shift total]
  where
    value LLeft = -1
    value LRight = 1
    value _ = error "This should never happen"

parseZero :: Parser Lex [Command]
parseZero = do
  _ <- one LOpen
  _ <- one LPlus <|> one LMinus
  _ <- one LClose
  return [Zero]

parseRead :: Parser Lex [Command]
parseRead = do
  _ <- one LRead
  return [Read]

parseWrite :: Parser Lex [Command]
parseWrite = do
  _ <- one LWrite
  return [Write]

parseStep1 :: Parser Lex [Command]
parseStep1 = parseAdd <|> parseShift <|> parseZero <|> parseRead <|> parseWrite <|> parseLoop
