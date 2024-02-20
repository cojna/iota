{-# OPTIONS_GHC -Wno-orphans #-}

module Data.SegTree.RangeUpdateRangeMin where

import Data.Coerce
import Data.Monoid.LastMin
import Data.SegTree
import Data.Semigroup

instance
  (Ord a, Bounded a) =>
  AsSemigroupEndo (Dual (LastMin a)) (Min a)
  where
  sendo x y
    | x == mempty = y
    | otherwise = coerce x
  {-# INLINE sendo #-}
