{-# OPTIONS_GHC -Wno-orphans #-}

module Data.SegTree.RangeMaxRangeMax where

import Data.SegTree
import Data.Semigroup

instance
  (Ord a, Bounded a) =>
  AsSemigroupEndo (Max a) (Max a)
  where
  sendo = (<>)
  {-# INLINE sendo #-}
