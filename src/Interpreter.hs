module Interpreter (interpret) where

import Data.Char (chr, ord)
import Data.Functor ((<&>))
import Data.Word (Word8)
import Optimizer

data Tape a = Tape ![a] !a ![a]

interpret :: [Command] -> IO (Maybe (Tape Word8))
interpret coms = do
  s <- getContents
  -- fst <$> runList startingTape coms s -- Apparently the same thing
  runList startingTape coms s <&> fst
  where
    startingTape = Tape [] 0 (repeat 0)

runList :: Tape Word8 -> [Command] -> String -> IO (Maybe (Tape Word8), String)
runList ta [] s = return (return ta, s)
runList ta (com : coms) s = do
  (mta, s') <- run ta com s
  case mta of
    Nothing -> return (Nothing, s')
    Just ta' -> runList ta' coms s'

-- TODO: Just use the IO monad to get the inputs too...
run :: Tape Word8 -> Command -> String -> IO (Maybe (Tape Word8), String)
run ta (Add n) s = return (return $ increment (fromIntegral n) ta, s)
run ta (Shift n) s = return (shiftBy n ta, s)
run ta Read (c : cs) = return (return $ set (fromIntegral $ ord c) ta, cs)
run ta Write s = putChar (chr . fromIntegral $ get ta) >> return (return ta, s)
run ta l@(LoopNZ coms) s =
  if get ta == 0
    then return (return ta, s)
    else do
      (mta, s') <- runList ta coms s
      case mta of
        Nothing -> return (Nothing, s')
        Just ta' -> run ta' l s'
run ta Zero s = return (return $ set 0 ta, s)
run ta (MoveNZ n) s =
  if v == 0
    then return (return ta, s)
    else return (mta, s)
  where
    v = get ta
    mta = do
      ta' <- shiftBy n (set 0 ta)
      shiftBy (- n) $ set v ta'
run ta (MoveTwoNZ n i) s = undefined
run ta (MoveMultNZ n i j) s = undefined
run ta (ScanFor wo n) s = undefined
run ta Read [] = error "Ran out of user input."

increment :: Num a => a -> Tape a -> Tape a
increment a (Tape lh x rh) = Tape lh (x + a) rh

set :: a -> Tape a -> Tape a
set n (Tape ls _ rs) = Tape ls n rs

get :: Tape a -> a
get (Tape _ x _) = x

leftShift :: Tape a -> Maybe (Tape a)
leftShift (Tape [] _ _) = Nothing
leftShift (Tape (l : ls) a rs) = Just $ Tape ls l (a : rs)

rightShift :: Tape a -> Maybe (Tape a)
rightShift (Tape _ _ []) = Nothing
rightShift (Tape ls a (r : rs)) = Just $ Tape (a : ls) r rs

shiftBy :: Int -> Tape a -> Maybe (Tape a)
shiftBy n ta = go (abs n) ta
  where
    shift = if n < 0 then leftShift else rightShift
    go 0 ta = Just ta
    go n ta = shift ta >>= go (n -1)
