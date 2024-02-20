{-# OPTIONS_GHC -Wno-orphans #-}

module Data.SegTree.RangeUpdateRangeMax where

import Data.Coerce
import Data.Monoid.LastMax
import Data.SegTree
import Data.Semigroup

instance
  (Ord a, Bounded a) =>
  AsSemigroupEndo (Dual (LastMax a)) (Max a)
  where
  sendo x y
    | x == mempty = y
    | otherwise = coerce x
  {-# INLINE sendo #-}
