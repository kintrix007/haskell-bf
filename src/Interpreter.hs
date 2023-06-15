module Interpreter (interpret) where

import Data.Char (chr, ord)
import Data.Word (Word8)
import Optimizer

data Tape a = Tape ![a] !a ![a]

interpret :: [Command] -> IO (Maybe (Tape Word8))
interpret coms = do
  runList startingTape coms
  where
    startingTape = Tape [] 0 (repeat 0)

runList :: Tape Word8 -> [Command] -> IO (Maybe (Tape Word8))
runList ta [] = return . return $ ta
runList ta (com : coms) = do
  mta <- run ta com
  case mta of
    Nothing -> return Nothing
    Just ta' -> runList ta' coms

run :: Tape Word8 -> Command -> IO (Maybe (Tape Word8))
run ta (Add n) = return . return $ increment (fromIntegral n) ta
run ta (Shift n) = return $ shiftBy n ta
run ta Read = do
  c <- getChar
  return . return $ set (fromIntegral $ ord c) ta
run ta Write = putChar (chr . fromIntegral $ get ta) >> (return . return $ ta)
run ta l@(LoopNZ coms) =
  if get ta == 0
    then return $ return ta
    else do
      mta <- runList ta coms
      case mta of
        Nothing -> return Nothing
        Just ta' -> run ta' l
run ta Zero = return . return $ set 0 ta
run ta (MoveNZ n) =
  if v == 0
    then return . return $ ta
    else return mta
  where
    v = get ta
    mta = do
      ta' <- shiftBy n (set 0 ta)
      shiftBy (- n) $ increment v ta'
run ta (MoveTwoNZ n m)
  | v == 0 = return . return $ ta
  | otherwise = return mta
  where
    v = get ta
    mta = do
      ta' <- shiftBy n (set 0 ta)
      ta'' <- shiftBy (m-n) (increment v ta')
      shiftBy (-m) (increment v ta'')
run ta move@(MoveMultNZ n d m)
  | v == 0 = return . return $ ta
  | v `rem` fromIntegral d == 0 = return mta
  | otherwise = do
      let mta' = do
            ta' <- mta
            ta'' <- shiftBy n (increment (fromIntegral (-d)) ta')
            shiftBy (-n) (increment (fromIntegral d) ta'')
      case mta' of
        Nothing -> return Nothing
        Just ta' -> run ta' move
  where
      v = get ta
      mta = do
        ta' <- shiftBy n (set 0 ta)
        shiftBy (- n) (increment (v `quot` fromIntegral d * fromIntegral m) ta')
run ta (ScanFor wo n) = undefined

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
