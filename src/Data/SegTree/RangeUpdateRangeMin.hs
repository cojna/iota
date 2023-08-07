{-# OPTIONS_GHC -Wno-orphans #-}

module Data.SegTree.RangeUpdateRangeMin where

import Data.Coerce
import Data.Monoid.LastMin
import Data.SegTree
import Data.Semigroup

instance
  (Eq a, Bounded a) =>
  MonoidAction (Dual (LastMin a)) (Min a)
  where
  appMonoid x y
    | x == mempty = y
    | otherwise = coerce x
  {-# INLINE appMonoid #-}
