module PingPong where

import Animation

data PingPong = PingPong Delta Delta deriving Show
ppCount (PingPong c _) = c
ppTop (PingPong _ top) = top

-- animate back and forth between 0 and 1 at some speed
pingPong :: A PingPong R
pingPong = goUp 0 (PingPong 0 1) where
  goUp dt (PingPong c top) = if c + dt < top
    then let c' = c + dt in A (PingPong c' top) (realToFrac . (/ top) . ppCount) goUp
    else goDown (dt - (top - c)) (PingPong top top) 
  goDown dt (PingPong c top) = if c - dt > 0
    then let c' = c - dt in A (PingPong c' top) (realToFrac . (/ top) . ppCount) goDown
    else goUp (dt - c) (PingPong 0 top)

setSpeed :: Rate -> PingPong -> PingPong
setSpeed speed (PingPong c top) =
  let top' = 1/speed in PingPong (c * top' / top) top'

