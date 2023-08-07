{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Monoid.LastMax where

import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U

newtype LastMax a = LastMax a
  deriving (Eq, Show, Bounded)

instance (Eq a, Bounded a) => Semigroup (LastMax a) where
  x <> y
    | y == minBound = x
    | otherwise = y

instance (Eq a, Bounded a) => Monoid (LastMax a) where
  mempty = minBound
  mconcat = last . (mempty :)

newtype instance U.MVector s (LastMax a) = MV_LastMax (U.MVector s a)
newtype instance U.Vector (LastMax a) = V_LastMax (U.Vector a)
deriving newtype instance (U.Unbox a) => GM.MVector U.MVector (LastMax a)
deriving newtype instance (U.Unbox a) => G.Vector U.Vector (LastMax a)
instance (U.Unbox a) => U.Unbox (LastMax a)
