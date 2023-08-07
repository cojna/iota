{-# OPTIONS_GHC -Wno-orphans #-}

module Data.SegTree.RangeMulRangeSum where

import Data.Coerce
import Data.SegTree
import Data.Semigroup

instance (Num a) => MonoidAction (Dual (Product a)) (Sum a) where
  appMonoid = coerce ((*) @a)
  {-# INLINE appMonoid #-}
