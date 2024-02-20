{-# OPTIONS_GHC -Wno-orphans #-}

module Data.SegTree.RangeAddRangeMax where

import Data.SegTree
import Data.Semigroup

instance
  (Num a, Ord a, Bounded a) =>
  AsSemigroupEndo (Sum a) (Max a)
  where
  sendo (Sum x) (Max y)
    | y /= minBound = Max (x + y)
    | otherwise = Max minBound
  {-# INLINE sendo #-}
