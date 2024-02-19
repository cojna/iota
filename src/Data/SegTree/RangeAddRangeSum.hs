{-# OPTIONS_GHC -Wno-orphans #-}

module Data.SegTree.RangeAddRangeSum where

import Data.Monoid.RangedSum
import Data.SegTree
import Data.Semigroup

instance (Num a) => AsSemigroupEndo (Sum a) (RangedSum a) where
  sendo (Sum x) (RangedSum len y) = RangedSum len (x * len + y)
  {-# INLINE sendo #-}
