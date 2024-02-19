{-# OPTIONS_GHC -Wno-orphans #-}

module Data.SegTree.RangeMulRangeSum where

import Data.Coerce
import Data.SegTree
import Data.Semigroup

instance (Num a) => AsSemigroupEndo (Dual (Product a)) (Sum a) where
  sendo = coerce ((*) @a)
  {-# INLINE sendo #-}
