{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.SegTree.RangeUpdateRangeMax where

import Data.Coerce
import Data.Monoid.LastMax
import Data.SegTree
import Data.Semigroup

instance
  (Eq a, Bounded a) =>
  MonoidAction (Dual (LastMax a)) (Max a)
  where
  appMonoid x y
    | x == mempty = y
    | otherwise = coerce x
  {-# INLINE appMonoid #-}
