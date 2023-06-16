module Optimizer (Command (..), optimize) where

import Control.Applicative
import Data.Foldable (Foldable (foldl'))
import Data.Word (Word8)
import Lex
import Parser

data Command
  = -- | Add n to the corrent cell's value.
    Add !Int
  | -- | Shift the "cell pointer" by n.
    Shift !Int
  | -- | Read for stdin.
    Read
  | -- | Write to stdout.
    Write
  | -- | Loop till the current cell's value is 0.
    LoopNZ ![Command]
  | -- | Make the current cell's value 0.
    Zero
  | -- | If the current cell's value is non-zero, move the value by n.
    MoveNZ !Int
  | -- | If the current cell's value is non-zero, move the value to two
    -- places, once by n and once by m.
    MoveTwoNZ !Int !Int
  | -- | If the current cell's value is non-zero, move the value by n whilst
    -- multiplying it by a fraction. If the current cell is not evenly divisible
    -- by the divisor, then it loops around.
    -- First param is offset then divisor then multiplier.
    MoveMultNZ !Int !Int !Int
  | -- | Scan for a given value in the in steps of n. It can be positive or
    -- negative to signify direction.
    ScanFor !Word8 !Int
  deriving (Show, Eq)

steps :: [Parser Command [Command]]
steps =
  [ parseZero <|> parseMove <|> parseMoveTwo <|> parseMoveMult
  , parseScanFor
  ]

optimize :: [Lex] -> Maybe [Command]
optimize lexs = do
  (cmds, []) <- parse (concat <$> many parseLex) lexs
  incrementalOptimization cmds

incrementalOptimization :: [Command] -> Maybe [Command]
incrementalOptimization cmds =
  foldl' (\acc x -> acc >>= idk x) (Just cmds) steps

idk :: Parser Command [Command] -> [Command] -> Maybe [Command]
idk p cmds = do
  (cmds', []) <- parse rep cmds
  return cmds'
  where
    rep = concat <$> many par
    par = p <|> innerAndRest rep

innerAndRest :: Parser Command [Command] -> Parser Command [Command]
innerAndRest parser = parseInnerLoops parser <|> ((: []) <$> item)

parseLex :: Parser Lex [Command]
parseLex = parseRead <|> parseWrite <|> parseShift <|> parseAdd <|> parseLoop

parseLoop :: Parser Lex [Command]
parseLoop = do
  _ <- one LOpen
  cmds <- concat <$> many parseLex
  _ <- one LClose
  return [LoopNZ cmds]

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

parseRead :: Parser Lex [Command]
parseRead = do
  _ <- one LRead
  return [Read]

parseWrite :: Parser Lex [Command]
parseWrite = do
  _ <- one LWrite
  return [Write]

parseZero :: Parser Command [Command]
parseZero = do
  LoopNZ [Add n] <- item
  parseWhen (abs n == 1) $ do
    return [Zero]

parseMove :: Parser Command [Command]
parseMove = do
  LoopNZ [Add (-1), Shift n, Add 1, Shift m] <- item
  parseWhen (n == (- m)) $ do
    return [MoveNZ n]

parseMoveTwo :: Parser Command [Command]
parseMoveTwo = do
  LoopNZ [Add (-1), Shift n, Add 1, Shift m, Add 1, Shift k] <- item
  parseWhen (n + m + k == 0) $ do
    return [MoveTwoNZ n (n + m)]

parseMoveMult :: Parser Command [Command]
parseMoveMult = do
  LoopNZ [Add divisor', Shift n, Add multiplier, Shift m] <- item
  let divisor = -divisor'
  parseWhen (n == (- m)) $ do
    return [MoveMultNZ n divisor multiplier]

parseScanFor :: Parser Command [Command]
parseScanFor = do
  Add n <- item
  LoopNZ [Add m, Shift sh, Add m'] <- item
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

parseInnerLoops :: Parser Command [Command] -> Parser Command [Command]
parseInnerLoops parser = do
  LoopNZ inner <- item
  case parse parser inner of
    -- Throws exceptions... Suboptimal, but if it happens then it's an error
    -- in the program. So it should be fine..?
    Nothing -> error "Parser failed inside a Loop. This should not happen."
    Just (cmds, left) ->
      if not (null left)
        then error "Parser stopped early inside a Loop. This should not happen."
        else return [LoopNZ cmds]
