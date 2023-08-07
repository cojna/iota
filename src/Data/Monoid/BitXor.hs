{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Monoid.BitXor where

import Data.Bits
import Data.Coerce (coerce)
import qualified Data.Foldable as F
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U

{- |
>>> mempty :: BitXor Int
BitXor {getBitXor = 0}
-}
newtype BitXor a = BitXor {getBitXor :: a}
  deriving (Eq, Show)

instance (Bits a) => Semigroup (BitXor a) where
  (<>) = coerce (xor @a)
  {-# INLINE (<>) #-}

instance (Bits a) => Monoid (BitXor a) where
  mempty = coerce (zeroBits @a)
  {-# INLINE mempty #-}
  mconcat = F.foldl' mappend mempty
  {-# INLINE mconcat #-}

newtype instance U.MVector s (BitXor a) = MV_BitXor (U.MVector s a)
newtype instance U.Vector (BitXor a) = V_BitXor (U.Vector a)
deriving newtype instance (U.Unbox a) => GM.MVector U.MVector (BitXor a)
deriving newtype instance (U.Unbox a) => G.Vector U.Vector (BitXor a)
instance (U.Unbox a) => U.Unbox (BitXor a)
