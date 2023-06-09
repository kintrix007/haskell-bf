module Optimizer (Command (..), optimize, optimizeStep1) where

import Control.Applicative
import Data.Word (Word8)
import Lex
import Parser

data Command
  = Add !Int
  | Shift !Int
  | Move !Int
  | Move2 !Int !Int
  | -- | First offset then divisor then multiplier
    MoveMult !Int !Int !Int
  | Zero
  | ScanFor !Word8 !Int
  | Loop ![Command]
  | Read
  | Write
  deriving (Show, Eq)

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither a Nothing = Left a
maybeToEither _ (Just x) = Right x

optimize :: [Lex] -> Either String [Command]
optimize lexs = do
  (cmds, left) <- maybeToEither "Source code malformed." $ parse optimizeStep1 lexs
  if not (null left)
    then Left $ "Parsing terminated early. Left: " ++ show left
    else do
      (cmds', left') <-
        maybeToEither "Step 2 optimization failed. This should not happen." $
          parse optimizeStep2 cmds
      if not (null left')
        then Left $ "Step 2 terminated early. Left: " ++ show left'
        else return cmds'

optimizeStep1 :: Parser Lex [Command]
optimizeStep1 = concat <$> many parseStep1

optimizeStep2 :: Parser Command [Command]
optimizeStep2 = concat <$> many parseStep2

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
  if n == (- m)
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
  if n == - (m + k) && n /= (- m)
    then return [Move2 n (n + m)]
    else empty

parseMoveMult :: Parser Lex [Command]
parseMoveMult = do
  _ <- one LOpen
  divisor <- length <$> some (one LMinus)
  [Shift n] <- parseShift
  multiplier <- length <$> some (one LPlus)
  [Shift m] <- parseShift
  _ <- one LClose
  if n == (- m)
    then return [MoveMult n divisor multiplier]
    else empty

parseStep1 :: Parser Lex [Command]
parseStep1 =
  parseRead
    <|> parseWrite
    <|> parseShift
    <|> parseAdd
    -- <|> parseZero
    -- <|> parseMove2
    -- <|> parseMove
    -- <|> parseMoveMult
    <|> parseLoop

parseScanFor :: Parser Command [Command]
parseScanFor = do
  Add n <- item
  Loop [Add m, Shift sh, Add m'] <- item
  Add n' <- item
  let diff = - m
  let firstAdd = n - diff
  let lastAdd = n' + diff

  if m == (- m')
    then
      return $
        [Add firstAdd | firstAdd /= 0]
          ++ [ScanFor (fromIntegral $ m `mod` 0xff) sh]
          ++ [Add lastAdd | lastAdd /= 0]
    else empty

parseInnerLoops :: Parser Command [Command]
parseInnerLoops = do
  Loop inner <- item
  case parse optimizeStep2 inner of
    -- Throws exceptions... Suboptimal, but if it happens then it's an error
    -- in the program. So it should be fine..?
    Nothing -> error "Parser failed inside a Loop. This should not happen."
    Just (cmds, left) ->
      if not (null left)
        then error "Parser stopped early inside a Loop. This should not happen."
        else return [Loop cmds]

parseStep2 :: Parser Command [Command]
parseStep2 =
  parseScanFor
    <|> parseInnerLoops
    <|> (: []) <$> item
