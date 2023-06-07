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
  | MoveMult !Int !Int -- ^First offset then multiplier
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
  if total == 0
    then return []
    else return [Add total]
  where
    value LPlus = 1
    value LMinus = -1
    value _ = error "This should never happen"

parseShift :: Parser Lex [Command]
parseShift = do
  xs <- some (one LLeft <|> one LRight)
  let total = sum . map value $ xs
  if total == 0
    then return []
    else return [Shift total]
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

parseMove :: Parser Lex [Command]
parseMove = do
  _ <- one LOpen
  _ <- one LMinus
  [Shift n] <- parseShift
  _ <- one LPlus
  [Shift m] <- parseShift
  _ <- one LClose
  if n == (-m)
    then return [Move n]
    else empty

parseMove2 :: Parser Lex [Command]
parseMove2 = do
  _ <- one LOpen
  _ <- one LMinus
  [Shift n] <- parseShift
  _ <- one LPlus
  [Shift m] <- parseShift
  _ <- one LPlus
  [Shift k] <- parseShift
  _ <- one LClose
  if n == -(m + k) && n /= (-m)
    then return [Move2 n (n+m)]
    else empty

parseMoveMult :: Parser Lex [Command]
parseMoveMult = do
  _ <- one LOpen
  _ <- one LMinus
  [Shift n] <- parseShift
  mult <- length <$> some (one LPlus)
  [Shift m] <- parseShift
  _ <- one LClose
  if n == (-m)
    then return [MoveMult n mult]
    else empty

parseStep1 :: Parser Lex [Command]
parseStep1 =
  parseAdd <|> parseShift <|> parseZero <|> parseRead <|> parseWrite
  <|> parseMove2 <|> parseMove <|> parseMoveMult <|> parseLoop
