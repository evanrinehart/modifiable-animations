{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
module Wrapper where

import Animation

type Evolve m v = Delta -> m -> A m v
type Handler f m v =
  Evolve m v -> Delta -> f m -> forall a . (A m v, a -> f a)

wrap :: Functor f => (m -> f m) -> Handler f m v -> A m v -> A (f m) (f v)
wrap mk0 h (A m0 v0 go0) = A (mk0 m0) (fmap v0) (go go0) where
  go innerGo dt m = A (mk m') (fmap v0) (go innerGo') where
    (A m' v' innerGo', mk) = h innerGo dt m
