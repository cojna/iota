{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.SegTree.RangeMulRangeSum where

import Data.Coerce
import Data.SegTree
import Data.Semigroup

instance (Num a) => MonoidAction (Product a) (Sum a) where
  appMonoid = coerce ((*) @a)
  {-# INLINE appMonoid #-}
