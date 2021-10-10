{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.SegTree.RangeAddRangeMin where

import Data.SegTree
import Data.Semigroup

instance
  (Num a, Ord a, Bounded a) =>
  MonoidAction (Sum a) (Min a)
  where
  appMonoid (Sum x) (Min y)
    | y /= maxBound = Min (x + y)
    | otherwise = Min maxBound
  {-# INLINE appMonoid #-}
