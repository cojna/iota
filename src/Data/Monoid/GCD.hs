{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Monoid.GCD where

import Data.Coerce (coerce)
import qualified Data.Foldable as F
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U

{- |
>>> mempty :: GCD Int
GCD {getGCD = 0}
>>> GCD (-4) <> GCD 6
GCD {getGCD = 2}
>>> GCD (-1) <> mempty
GCD {getGCD = 1}
-}
newtype GCD a = GCD {getGCD :: a}
  deriving (Eq, Show)

instance (Integral a) => Semigroup (GCD a) where
  (<>) = coerce (gcd @a)
  {-# INLINE (<>) #-}

instance (Num a, Integral a) => Monoid (GCD a) where
  mempty = GCD 0
  {-# INLINE mempty #-}
  mconcat = F.foldl' mappend mempty
  {-# INLINE mconcat #-}

newtype instance U.MVector s (GCD a) = MV_GCD (U.MVector s a)
newtype instance U.Vector (GCD a) = V_GCD (U.Vector a)
deriving newtype instance (U.Unbox a) => GM.MVector U.MVector (GCD a)
deriving newtype instance (U.Unbox a) => G.Vector U.Vector (GCD a)
instance (U.Unbox a) => U.Unbox (GCD a)
