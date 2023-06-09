module Interpreter (interpret, RunState (..), RunResult (..), InOut (..)) where

import Data.Array
import Data.Char
import Data.Word (Word8)
import Optimizer (Command (..))

type Memory = Array Int Word8

data RunState = RunSuccess | RunFailure !String

data RunResult = RunResult
  { idx :: !Int,
    memory :: !Memory,
    inOut :: !InOut,
    state :: !RunState
  }

data InOut = InOut !String !String

startingArray :: Memory
startingArray = array bound $ zip [fst bound .. snd bound] (repeat 0)
  where
    bound = (1, 1000)

interpret :: String -> [Command] -> RunResult
interpret io [] = RunResult 1 startingArray (InOut io "") RunSuccess
interpret io coms = do
  runLoop coms (InOut io "") 1 startingArray

run :: Command -> InOut -> Int -> Array Int Word8 -> RunResult
run (Add i) io n ar = RunResult n (ar // [(n, (ar ! n) + fromIntegral i)]) io RunSuccess
run (Shift i) io n ar
  | not $ inRange (bounds ar) n = RunResult n ar io (RunFailure "Out of bounds")
  | otherwise = RunResult (n + i) ar io RunSuccess
run (Move i) io n ar
  | ar ! n == 0 = RunResult n ar io RunSuccess
  | not $ inRange (bounds ar) (n + i) = RunResult n ar io (RunFailure "Out of bounds")
  | otherwise = RunResult n (ar // [(n, 0), (n + i, (ar ! (n + i)) + (ar ! n))]) io RunSuccess
run (Move2 i j) io n ar
  | ar ! n == 0 = RunResult n ar io RunSuccess
  | not $ inRange (bounds ar) (n + i) = RunResult n ar io (RunFailure "Out of bounds")
  | not $ inRange (bounds ar) (n + j) = RunResult n ar io (RunFailure "Out of bounds")
  | otherwise = RunResult n (ar // [(n, 0), (n + i, ar ! (n + i) + ar ! n), (n + j, ar ! (n + j) + ar ! n)]) io RunSuccess
run (MoveMult i d m) io n ar
  | ar ! n == 0 = RunResult n ar io RunSuccess
  | (ar ! n) `rem` fromIntegral d == 0 =
    RunResult n (ar // [(n, 0), (n + i, ar ! (n + i) + newVal)]) io RunSuccess
  | otherwise = run (Loop []) io n ar -- aka: hang
  where
    newVal = (ar ! n) `quot` fromIntegral d * fromIntegral m
run Zero inp n ar = RunResult n (ar // [(n, 0)]) inp RunSuccess
run (ScanFor wo i) inp n ar
  | not $ inRange (bounds ar) n = RunResult n ar inp (RunFailure "Out of bounds")
  | otherwise =
    if ar ! n == wo
      then RunResult n ar inp RunSuccess
      else run (ScanFor wo i) inp (n + i) ar
run (Loop coms) io n ar =
  if ar ! n == 0
    then RunResult n ar io RunSuccess
    else do
      let RunResult ar' n' io' res = runLoop coms io n ar
       in case res of
            (RunFailure _) -> RunResult ar' n' io' res
            RunSuccess -> run (Loop coms) io' ar' n'
run Read (InOut inp out) n ar = RunResult n (ar // [(n, ch)]) (InOut (tail inp) out) RunSuccess
  where
    ch = fromIntegral $ ord (head inp)
run Write (InOut inp out) n ar = RunResult n ar (InOut inp (out ++ [ch])) RunSuccess
  where
    ch = chr (fromIntegral (ar ! n))

runLoop :: [Command] -> InOut -> Int -> Memory -> RunResult
runLoop [] io n ar = RunResult n ar io RunSuccess
runLoop (com : coms) io n ar = do
  let RunResult n' ar' io' res = run com io n ar
   in case res of
        (RunFailure _) -> RunResult n' ar' io' res
        RunSuccess -> runLoop coms io' n' ar'
