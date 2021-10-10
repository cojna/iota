{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.SegTree.RangeAddRangeSum where

import Data.Monoid.RangedSum
import Data.SegTree
import Data.Semigroup

instance (Num a) => MonoidAction (Sum a) (RangedSum a) where
  appMonoid (Sum x) (RangedSum len y) = RangedSum len (x * len + y)
  {-# INLINE appMonoid #-}
