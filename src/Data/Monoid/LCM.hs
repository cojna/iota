{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Monoid.LCM where

import Data.Coerce (coerce)
import qualified Data.Foldable as F
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U

{- |
>>> mempty :: LCM Int
LCM {getLCM = 1}
>>> LCM (-2) <> LCM 3
LCM {getLCM = 6}
>>> LCM (-1) <> mempty
LCM {getLCM = 1}
-}
newtype LCM a = LCM {getLCM :: a}
  deriving (Eq, Show)

instance (Integral a) => Semigroup (LCM a) where
  (<>) = coerce (lcm @a)
  {-# INLINE (<>) #-}

instance (Num a, Integral a) => Monoid (LCM a) where
  mempty = LCM 1
  {-# INLINE mempty #-}
  mconcat = F.foldl' mappend mempty
  {-# INLINE mconcat #-}

newtype instance U.MVector s (LCM a) = MV_LCM (U.MVector s a)
newtype instance U.Vector (LCM a) = V_LCM (U.Vector a)
deriving newtype instance (U.Unbox a) => GM.MVector U.MVector (LCM a)
deriving newtype instance (U.Unbox a) => G.Vector U.Vector (LCM a)
instance (U.Unbox a) => U.Unbox (LCM a)
