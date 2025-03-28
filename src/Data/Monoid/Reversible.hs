{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Monoid.Reversible where

import Data.Semigroup
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

data Reversible a = Reversible !a !(Dual a)

runReversible :: Reversible a -> a
runReversible (Reversible x _) = x

reversible :: a -> Reversible a
reversible x = Reversible x (Dual x)

{- |
/O(1)/

>>> mreverse $ foldMap (reversible . (:[])) "abc"
"cba"
>>> mreverse $ reversible "abc"
"abc"
>>> mreverse $ reversible "abc" <> reversible "def"
"defabc"
-}
mreverse :: Reversible a -> Reversible a
mreverse (Reversible x (Dual y)) = Reversible y (Dual x)

instance (Eq a) => Eq (Reversible a) where
  (Reversible x _) == (Reversible y _) = x == y

instance (Ord a) => Ord (Reversible a) where
  compare (Reversible x _) (Reversible y _) = compare x y

instance (Show a) => Show (Reversible a) where
  show = show . runReversible

instance (Semigroup a) => Semigroup (Reversible a) where
  (Reversible x x') <> (Reversible y y') = Reversible (x <> y) (x' <> y')

instance (Monoid a) => Monoid (Reversible a) where
  mempty = Reversible mempty mempty

instance (U.Unbox a) => U.IsoUnbox (Reversible a) (a, a) where
  toURepr (Reversible x (Dual y)) = (x, y)
  fromURepr (x, y) = Reversible x (Dual y)

newtype instance UM.MVector s (Reversible a) = MV_Reversible (UM.MVector s (a, a))
newtype instance U.Vector (Reversible a) = V_Reversible (U.Vector (a, a))
deriving via (Reversible a `U.As` (a, a)) instance (U.Unbox a) => GM.MVector U.MVector (Reversible a)
deriving via (Reversible a `U.As` (a, a)) instance (U.Unbox a) => G.Vector U.Vector (Reversible a)
instance (U.Unbox a) => U.Unbox (Reversible a)
