{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.SegTree.RangeMaxRangeMax where

import Data.SegTree
import Data.Semigroup

instance
  (Ord a, Bounded a) =>
  MonoidAction (Max a) (Max a)
  where
  appMonoid = (<>)
  {-# INLINE appMonoid #-}
