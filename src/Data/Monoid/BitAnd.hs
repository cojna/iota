{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Monoid.BitAnd where

import Data.Bits
import Data.Coerce (coerce)
import qualified Data.Foldable as F
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U

{- |
>>> mempty :: BitAnd Int
BitAnd {getBitAnd = -1}
>>> mempty :: BitAnd Word
BitAnd {getBitAnd = 18446744073709551615}
-}
newtype BitAnd a = BitAnd {getBitAnd :: a}
  deriving (Eq, Show)

instance (Bits a) => Semigroup (BitAnd a) where
  (<>) = coerce ((.&.) @a)
  {-# INLINE (<>) #-}

instance (Bits a) => Monoid (BitAnd a) where
  mempty = coerce $ complement (zeroBits @a)
  {-# INLINE mempty #-}
  mconcat = F.foldl' mappend mempty
  {-# INLINE mconcat #-}

newtype instance U.MVector s (BitAnd a) = MV_BitAnd (U.MVector s a)
newtype instance U.Vector (BitAnd a) = V_BitAnd (U.Vector a)
deriving newtype instance (U.Unbox a) => GM.MVector U.MVector (BitAnd a)
deriving newtype instance (U.Unbox a) => G.Vector U.Vector (BitAnd a)
instance (U.Unbox a) => U.Unbox (BitAnd a)
