{-# OPTIONS_GHC -Wno-orphans #-}

module Data.SegTree.RangeAddRangeMin where

import Data.SegTree
import Data.Semigroup

instance
  (Num a, Ord a, Bounded a) =>
  AsSemigroupEndo (Sum a) (Min a)
  where
  sendo (Sum x) (Min y)
    | y /= maxBound = Min (x + y)
    | otherwise = Min maxBound
  {-# INLINE sendo #-}
