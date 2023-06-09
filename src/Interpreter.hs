module Interpreter (interpret) where

import Data.Array
import Data.Char
import Data.Word (Word8)
import Debug.Trace (trace)
import Optimizer (Command (..))

type Memory = Array Int Word8

data RunState = RunSuccess | RunFailure !String

data RunResult = RunResult
  { idx :: !Int,
    memory :: !Memory,
    input :: !String,
    output :: !String,
    state :: !RunState
  }

startingArray :: Memory
startingArray = array bound $ zip [fst bound .. snd bound] (repeat 0)
  where
    bound = (1, 1000)

interpret :: String -> [Command] -> (Memory, String, RunState)
interpret _ [] = (startingArray, "", RunSuccess)
interpret inp coms = do
  let (_, ar, _, runRes) = runLoop coms inp 1 startingArray
   in (ar, "", runRes)

run :: Command -> String -> Int -> Array Int Word8 -> (Int, Memory, String, RunState)
run (Add i) inp n ar = (n, ar // [(n, (ar ! n) + fromIntegral i)], inp, RunSuccess)
run (Shift i) inp n ar
  | not $ inRange (bounds ar) n = (n, ar, inp, RunFailure "Out of bounds")
  | otherwise = (n + i, ar, inp, RunSuccess)
run (Move i) inp n ar
  | ar ! n == 0 = (n, ar, inp, RunSuccess)
  | not $ inRange (bounds ar) (n + i) = (n, ar, inp, RunFailure "Out of bounds")
  | otherwise = (n, ar // [(n, 0), (n + i, (ar ! (n + i)) + (ar ! n))], inp, RunSuccess)
run (Move2 i j) inp n ar
  | ar ! n == 0 = (n, ar, inp, RunSuccess)
  | not $ inRange (bounds ar) (n + i) = (n, ar, inp, RunFailure "Out of bounds")
  | not $ inRange (bounds ar) (n + j) = (n, ar, inp, RunFailure "Out of bounds")
  | otherwise = (n, ar // [(n, 0), (n + i, ar ! (n + i) + ar ! n), (n + j, ar ! (n + j) + ar ! n)], inp, RunSuccess)
run (MoveMult i d m) inp n ar
  | ar ! n == 0 = (n, ar, inp, RunSuccess)
  | (ar ! n) `rem` fromIntegral d == 0 =
    (n, ar // [(n, 0), (n + i, ar ! (n + i) + newVal)], inp, RunSuccess)
  | otherwise = run (Loop []) inp n ar -- aka: hang
  where
    newVal = (ar ! n) `quot` fromIntegral d * fromIntegral m
run (Copy i) inp n ar
  | ar ! n == 0 = (n, ar, inp, RunSuccess)
  | not $ inRange (bounds ar) (n + i) = (n, ar, inp, RunFailure "Out of bounds")
  | otherwise = (n, ar // [(n + i, ar ! (n + i) + ar ! n)], inp, RunSuccess)
run Zero inp n ar = (n, ar // [(n, 0)], inp, RunSuccess)
run (ScanFor wo i) inp n ar
  | not $ inRange (bounds ar) n = (n, ar, inp, RunFailure "Out of bounds")
  | otherwise =
    if ar ! n == wo
      then (n, ar, inp, RunSuccess)
      else run (ScanFor wo i) inp (n + i) ar
run (Loop coms) inp n ar =
  if ar ! n == 0
    then (n, ar, inp, RunSuccess)
    else do
      let (ar', n', inp', res) = runLoop coms inp n ar
       in case res of
            (RunFailure _) -> (ar', n', inp', res)
            RunSuccess -> run (Loop coms) inp' ar' n'
run Read inp n ar = undefined
run Write inp n ar = (n, ar, inp, RunSuccess)

runLoop :: [Command] -> String -> Int -> Memory -> (Int, Memory, String, RunState)
runLoop [] inp n ar = (n, ar, inp, RunSuccess)
runLoop (com : coms) inp n ar = do
  let (n', ar', inp', res) = run com inp n ar
   in case res of
        (RunFailure _) -> (n', ar', inp', res)
        RunSuccess -> runLoop coms inp n' ar'
