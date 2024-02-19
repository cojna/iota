{-# OPTIONS_GHC -Wno-orphans #-}

module Data.SegTree.RangeAffineRangeSum where

import Data.Monoid.Affine
import Data.Monoid.RangedSum
import Data.SegTree

instance (Num a) => AsSemigroupEndo (Affine a) (RangedSum a) where
  sendo (Affine a b) (RangedSum len x) = RangedSum len (a * x + len * b)
  {-# INLINE sendo #-}
