{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
module Animation where

import Data.Fixed
import qualified Data.Map as M
import Data.Bifunctor
import Text.Show.Functions
import Control.Concurrent
import Control.Monad

type Delta = Pico 
type Time = Delta
type Rate = Delta
type R = Double
type R2 = (R,R)

data A m v = A m (m -> v) (m -> Delta -> A m v)

view :: A m v -> v
view (A m view _) = view m

model :: A m v -> m
model (A m _ _) = m

advance :: Delta -> A m v -> A m v
advance dt (A m view go) = go m dt

modify :: (m -> m) -> A m v -> A m v
modify f (A m view go) = A (f m) view go

mapView :: (u -> v) -> A m u -> A m v
mapView f (A m view go) = A m (fmap f view) goo where
  goo dt m =
    let A m' view' go' = go dt m in
    A m' (fmap f view') goo

isoModel :: (m -> n) -> (n -> m) -> A m v -> A n v
isoModel f g (A m view go0) = A (f m) (view . g) (go go0) where
  go innerGo n dt =
    let A m' view' go' = innerGo (g n) dt in
    A (f m') (view' . g) (go go')
    
instance Functor (A m) where
  fmap = mapView

instance (Show m, Show v) => Show (A m v) where
  show (A x view advance) = show x ++ " >- " ++ show (view x)

collect :: (forall a b . (a -> b) -> f a -> f b) -> f (A m v) -> A (f (A m v)) (f v)
collect map start = A start (map view) go where
  go m dt = let m' = map (advance dt) m in A m' (map view) go

collect2 :: (forall a b c d . (a -> c) -> (b -> d) -> f a b -> f c d) ->
            f (A m1 v1) (A m2 v2) ->
            A (f (A m1 v1) (A m2 v2)) (f v1 v2) 
collect2 map start = A start (map view view) go where
  go m dt = let m' = map (advance dt) (advance dt) m in A m' (map view view) go

barn :: M.Map Int (A m v) -> A (M.Map Int (A m v)) (M.Map Int v)
barn = collect M.map

fuse :: A m1 v1 -> A m2 v2 -> A (A m1 v1, A m2 v2) (v1,v2)
fuse a1 a2 = collect2 bimap (a1,a2)

lift :: Functor f => f (A m v) -> A (f (A m v)) (f v)
lift = collect fmap

still :: v -> A v v
still x = A x id go where
  go x dt = A x id go

clock :: Time -> A Time Time
clock t0 = A t0 id go where
  go t dt = A (t + dt) id go

anim :: (m -> Delta -> m) -> m -> A m m
anim f m0 = go m0 0 where
  go m dt = let m' = f m dt in A m' id go

circle :: A Time R2
circle = mapView ((\t -> (cos t, sin t)) . realToFrac) (clock 0)

data Segs m a = End (A m a) | Segs Time (A m a) (m -> Segs m a)

instance (Show m, Show a) => Show (Segs m a) where
  show (End a) = "End (" ++ show a ++ ")"
  show (Segs t a mkss) = "Segs (" ++ show t ++ " " ++ show a ++ " ; ... )"

peek :: Segs m a -> a
peek (End a) = view a
peek (Segs t a _) = view a

segments :: Segs m v -> A (Segs m v) v
segments segs = A segs peek go where
  go (End a) dt = let a' = advance dt a in A (End a') peek go
  go (Segs t a mkss) dt = if dt < t
    then A (Segs (t - dt) (advance dt a) mkss) peek go
    else
      let A m _ _ = advance t a in
      go (mkss m) (dt - t)

data DetectTime a = NotBefore Time | At Time a deriving Show

detect :: (m -> DetectTime a) -> (a -> m -> m) -> A m v -> A m v
detect d f (A m view go0) = A m view (go go0) where
  check x@(NotBefore t) | t < 0 = error ("NotBefore " ++ show t ++ " invalid")
                        | otherwise = x
  check x@(At t _) | t < 0 = error ("At " ++ show t ++ " invalid")
                   | otherwise = x
  go innerGo m dt = case check (d m) of
    NotBefore t -> if dt < t
      then let A m' view' go' = innerGo m dt in A m' view' (go go')
      else let A m' view' go' = innerGo m t in go go' m' (dt - t)
    At t x -> if dt < t
      then let A m' view' go' = innerGo m dt in A m' view' (go go')
      else
        let A m' view' go' = innerGo m t in
        let m'' = f x m' in
        go go' m'' (dt - t)

pingPong :: A (Delta,Delta) R
pingPong = goUp (0,1) 0 where
  goUp (c,top) dt = if c + dt < top
    then let c' = c + dt in A (c',top) (realToFrac . (/ top) . fst) goUp
    else goDown (top,top) (dt - (top - c))
  goDown (c,top) dt = if c - dt > 0
    then let c' = c - dt in A (c',top) (realToFrac . (/ top) . fst) goDown
    else goUp (0,top) (dt - c)

setSlowness :: Delta -> (Delta,Delta) -> (Delta,Delta)
setSlowness top' (c, top) = (c * top' / top, top')

ray :: R -> R -> A (R,R) R
ray x0 v0 = go (x0,v0) 0 where
  go (x,v) dt = let x' = x + v * realToFrac dt in A (x',v) fst go

left :: (a -> b) -> (a,c) -> (b,c)
left f (x,y) = (f x, y)

right :: (a -> b) -> (c,a) -> (c,b)
right f (x,y) = (x, f y)

key :: Ord k => k -> (a -> a) -> M.Map k a -> M.Map k a
key k f = M.adjust f k

seg :: (A m v -> A m v) -> Segs m v -> Segs m v
seg f (End a) = End (f a)
seg f (Segs t a ss) = Segs t (f a) ss

{-
data AddValue v a = AddValue v a deriving (Show, Functor)
data AddMotion a = AddMotion R2 R2 a deriving (Show, Functor)
data AddAlt a b = Alt1 a b | Alt2 a b deriving Show -- Bifunctor
data AddPause a = Unpaused a | Paused a deriving (Show, Functor)

addMotion :: A m v -> A (AddMotion (A m v)) (AddMotion v)
addValue :: a -> A m v -> A (AddValue a (A m v)) (AddValue v)
addAlt :: A m v -> A n w -> A (AddAlt (A m v) (A n w)) (AddAlt v w)
addPause :: A m v -> A (AddPause (A m v)) (AddPause v)

alt1 :: AddAlt a b -> AddAlt a b
alt2 :: AddAlt a b -> AddAlt a b
toggle :: AddAlt a b -> AddAlt a b
pause :: AddPause a -> AddPause a
unpause :: AddPause a -> AddPause a
-}

class Payload f where
  payload :: f a -> a

-- pacmans components
-- chewing action, pausable 
