{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.SegTree.RangeAddRangeMax where

import Data.SegTree
import Data.Semigroup

instance
  (Num a, Ord a, Bounded a) =>
  MonoidAction (Sum a) (Max a)
  where
  appMonoid (Sum x) (Max y)
    | y /= minBound = Max (x + y)
    | otherwise = Max minBound
  {-# INLINE appMonoid #-}
