{-# OPTIONS_GHC -Wno-orphans #-}

module Data.SegTree.RangeAffineRangeSum where

import Data.Monoid.Affine
import Data.Monoid.RangedSum
import Data.SegTree

instance (Num a) => MonoidAction (Affine a) (RangedSum a) where
  appMonoid (Affine a b) (RangedSum len x) = RangedSum len (a * x + len * b)
  {-# INLINE appMonoid #-}
