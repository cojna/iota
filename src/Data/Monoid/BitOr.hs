{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Monoid.BitOr where

import Data.Bits
import Data.Coerce (coerce)
import qualified Data.Foldable as F
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U

{- |
>>> mempty :: BitOr Int
BitOr {getBitOr = 0}
-}
newtype BitOr a = BitOr {getBitOr :: a}
  deriving (Eq, Show)

instance (Bits a) => Semigroup (BitOr a) where
  (<>) = coerce ((.|.) @a)
  {-# INLINE (<>) #-}

instance (Bits a) => Monoid (BitOr a) where
  mempty = coerce (zeroBits @a)
  {-# INLINE mempty #-}
  mconcat = F.foldl' mappend mempty
  {-# INLINE mconcat #-}

newtype instance U.MVector s (BitOr a) = MV_BitOr (U.MVector s a)
newtype instance U.Vector (BitOr a) = V_BitOr (U.Vector a)
deriving newtype instance (U.Unbox a) => GM.MVector U.MVector (BitOr a)
deriving newtype instance (U.Unbox a) => G.Vector U.Vector (BitOr a)
instance (U.Unbox a) => U.Unbox (BitOr a)
