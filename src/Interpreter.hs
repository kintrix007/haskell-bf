module Interpreter (interpret) where

import Data.Array
import Data.Word (Word8)
import Optimizer (Command (..))
import Debug.Trace (trace)
import Data.Char

startingArray :: Array Int Word8
startingArray = array bound $ zip [fst bound .. snd bound] (repeat 0)
  where
    bound = (1, 1000)

interpret :: String -> [Command] -> Maybe (String, Array Int Word8)
interpret _inputs [] = Just ("", startingArray)
interpret _inputs coms = do
  (_, ar) <- runLoop coms 1 startingArray
  Just ("", ar)

run :: Command -> Int -> Array Int Word8 -> Maybe (Int, Array Int Word8)
run (Add i) n ar = Just (n, ar // [(n, (ar ! n) + fromIntegral i)])
run (Shift i) n ar
  | not $ inRange (bounds ar) n = Nothing
  | otherwise = Just (n + i, ar)
run (Move i) n ar
  | not $ inRange (bounds ar) (n + i) = Nothing
  | otherwise = Just (n, ar // [(n, 0), (n + i, ar ! n)])
run (Move2 i j) n ar
  | not $ inRange (bounds ar) (n + i) = Nothing
  | not $ inRange (bounds ar) (n + j) = Nothing
  | otherwise = Just (n, ar // [(n, 0), (n + i, ar ! n), (n + j, ar ! n)])
run (MoveMult i d m) n ar
  | (ar ! n) `rem` fromIntegral d == 0 =
    Just (n, ar // [(n, 0), (n + i, newVal)])
  | otherwise = run (Loop []) n ar -- aka: hang
  where
    newVal = (ar ! n) `quot` fromIntegral d * fromIntegral m
run (Copy i) n ar
  | not $ inRange (bounds ar) (n + i) = Nothing
  | otherwise = Just (n, ar // [(n + i, ar ! n)])
run Zero n ar =
  Just (n, ar // [(n, 0)])
run (ScanFor wo i) n ar
  | not $ inRange (bounds ar) n = Nothing
  | otherwise =
    if ar ! n == wo
      then Just (n, ar)
      else run (ScanFor wo i) (n + i) ar
run (Loop coms) n ar =
  if ar ! n == 0
    then Just (n, ar)
    else do
      (ar', n') <- runLoop coms n ar
      run (Loop coms) ar' n'
run Read n ar = undefined
run Write n ar =
  trace [chr $ fromIntegral (ar ! n)] Just (n, ar)
  -- Just (n, ar)

runLoop :: [Command] -> Int -> Array Int Word8 -> Maybe (Int, Array Int Word8)
runLoop [] n ar = Just (n, ar)
runLoop (com : coms) n ar = do
  (n', ar') <- run com n ar
  runLoop coms n' ar'
