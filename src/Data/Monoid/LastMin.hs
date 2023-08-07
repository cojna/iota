{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Monoid.LastMin where

import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U

newtype LastMin a = LastMin a
  deriving (Eq, Show, Bounded)

instance (Eq a, Bounded a) => Semigroup (LastMin a) where
  x <> y
    | y == maxBound = x
    | otherwise = y

instance (Eq a, Bounded a) => Monoid (LastMin a) where
  mempty = maxBound
  mconcat = last . (mempty :)

newtype instance U.MVector s (LastMin a) = MV_LastMin (U.MVector s a)
newtype instance U.Vector (LastMin a) = V_LastMin (U.Vector a)
deriving newtype instance (U.Unbox a) => GM.MVector U.MVector (LastMin a)
deriving newtype instance (U.Unbox a) => G.Vector U.Vector (LastMin a)
instance (U.Unbox a) => U.Unbox (LastMin a)
