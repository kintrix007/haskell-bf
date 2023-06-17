module Optimizer (Command (..), optimize) where

import           Control.Applicative
import           Data.Foldable       (Foldable (foldl'))
import           Lex
import           Parser

data Command
  = Add !Int
  -- ^ Add n to the corrent cell's value.
  | Shift !Int
  -- ^ Shift the "cell pointer" by n.
  | Read
  -- ^ Read for stdin.
  | Write
  -- ^ Write to stdout.
  | LoopNZ ![Command]
  -- ^ Loop till the current cell's value is 0.
  | Zero
  -- ^ Make the current cell's value 0.
  | MoveNZ !Int
  -- ^ If the current cell's value is non-zero, move the value by n.
  | MoveTwoNZ !Int !Int
  -- ^ If the current cell's value is non-zero, move the value to two
  -- places, once by n and once by m.
  | MoveMultNZ !Int !Int !Int
  -- ^ If the current cell's value is non-zero, move the value by n whilst
  -- multiplying it by a fraction. If the current cell is not evenly divisible
  -- by the divisor, then it loops around.
  -- First param is offset then divisor then multiplier.
  | ZeroingIf ![Command]
  -- ^ Zero the current 
  deriving (Show, Eq)

steps :: [Parser Command [Command]]
steps =
  [ parseComment
  , parseZero <|> parseMoveTwo <|> parseMoveMult
  , parseMove
  ]

optimize :: [Lex] -> Maybe [Command]
optimize lexs = do
  (cmds, []) <- parse (concat <$> many parseLex) lexs
  incrementalOptimization cmds

incrementalOptimization :: [Command] -> Maybe [Command]
incrementalOptimization cmds =
  foldl' (\acc x -> acc >>= parseWithInner x) (Just cmds) steps

parseWithInner :: Parser Command [Command] -> [Command] -> Maybe [Command]
parseWithInner p cmds = do
  (cmds', []) <- parse rep cmds
  return cmds'
  where
    rep = concat <$> many par
    par = p <|> innerAndRest rep

innerAndRest :: Parser Command [Command] -> Parser Command [Command]
innerAndRest parser = parseInnerLoops parser <|> ((:[]) <$> item)

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
    value LPlus  = 1
    value LMinus = -1
    value _      = error "This should never happen"

parseShift :: Parser Lex [Command]
parseShift = do
  xs <- some (one LLeft <|> one LRight)
  let total = sum . map value $ xs
  if total == 0
    then return []
    else return [Shift total]
  where
    value LLeft  = -1
    value LRight = 1
    value _      = error "This should never happen"

parseRead :: Parser Lex [Command]
parseRead = one LRead >> return [Read]

parseWrite :: Parser Lex [Command]
parseWrite = one LWrite >> return [Write]

parseComment :: Parser Command [Command]
parseComment = do
  LoopNZ coms <- item
  _ <- some $ do
    LoopNZ _ <- item
    return ()
  return [LoopNZ coms]

parseZero :: Parser Command [Command]
parseZero = do
  LoopNZ [Add n] <- item
  parseWhen (abs n == 1) $ do
    return [Zero]

parseMoveTwo :: Parser Command [Command]
parseMoveTwo = do
  (n, m, k) <- do
    do
      LoopNZ [Add (-1), Shift n, Add 1, Shift m, Add 1, Shift k] <- item
      return (n, m, k)
    <|>
    do
      LoopNZ [Shift n, Add 1, Shift m, Add 1, Shift k, Add (-1)] <- item
      return (n, m, k)
  parseWhen (n + m + k == 0) $ do
    return [MoveTwoNZ n (n + m)]

parseMoveMult :: Parser Command [Command]
parseMoveMult = do
  ((n, m), (divisor, multiplier)) <- do
    do
      LoopNZ [Add divisor, Shift n, Add multiplier, Shift m] <- item
      return ((n, m), (-divisor, multiplier))
    <|>
    do
      LoopNZ [Shift n, Add multiplier, Shift m, Add divisor] <- item
      return ((n, m), (-divisor, multiplier))
  parseWhen (n == (-m)) $ do
    return [MoveMultNZ n divisor multiplier]

parseMove :: Parser Command [Command]
parseMove = do
  MoveMultNZ n d m <- item
  parseWhen (d == 1 && m == 1) $ do
    return [MoveNZ n]

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
