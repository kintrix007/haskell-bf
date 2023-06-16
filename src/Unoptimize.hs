module Unoptimize (unoptimize) where

import Optimizer (Command (..))

unoptimize :: Command -> [Command]
unoptimize (Add n) = [Add n]
unoptimize (Shift n) = [Shift n]
unoptimize Read = [Read]
unoptimize Write = [Write]
unoptimize (LoopNZ coms) = [LoopNZ coms]
unoptimize Zero = [LoopNZ [Add (-1)]]
unoptimize (MoveNZ n) = [LoopNZ [Add (-1), Shift n, Add 1, Shift (- n)]]
unoptimize (MoveTwoNZ n m) = [LoopNZ [Add (-1), Shift n, Add 1, Shift (m - n), Add 1, Shift (- m)]]
unoptimize (MoveMultNZ n d m) = [LoopNZ [Add (- d), Shift n, Add m, Shift (- n)]]
unoptimize (ScanFor wo n) = [Add (- x), LoopNZ [Add x, Shift n, Add (- x)], Add x]
  where
    x = fromIntegral (maxBound - wo)
