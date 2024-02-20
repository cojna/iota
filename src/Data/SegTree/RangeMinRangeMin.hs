{-# OPTIONS_GHC -Wno-orphans #-}

module Data.SegTree.RangeMinRangeMin where

import Data.SegTree
import Data.Semigroup

instance
  (Ord a, Bounded a) =>
  AsSemigroupEndo (Min a) (Min a)
  where
  sendo = (<>)
  {-# INLINE sendo #-}
