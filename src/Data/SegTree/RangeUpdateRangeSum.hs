{-# LANGUAGE PatternSynonyms #-}

module Data.SegTree.RangeUpdateRangeSum where

import Data.Monoid.Affine
import Data.SegTree.RangeAffineRangeSum ()

{- |
>>> import Data.SegTree
>>> import Data.Monoid.RangedSum
>>> sendo (LastSum @Int 999) (RangedSum @Int 1 123)
RangedSum {getRangesSumSize = 1, getRangedSum = 999}
-}
pattern LastSum :: (Integral a) => a -> Affine a
pattern LastSum x = Affine 0 x
