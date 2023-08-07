{-# OPTIONS_GHC -Wno-orphans #-}

module Data.SegTree.RangeMinRangeMin where

import Data.SegTree
import Data.Semigroup

instance
  (Ord a, Bounded a) =>
  MonoidAction (Min a) (Min a)
  where
  appMonoid = (<>)
  {-# INLINE appMonoid #-}
