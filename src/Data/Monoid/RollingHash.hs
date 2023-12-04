{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Monoid.RollingHash where

import Data.Char
import Data.Coerce
import qualified Data.Foldable as F
import Data.Monoid
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import GHC.Exts
import GHC.TypeLits

import Data.Monoid.Affine
import Data.RollingHash

{- |
@b@ should be a primitive root of @2^61-1@

>>> :set -XDataKinds -XOverloadedStrings
>>> runRollingHashBuilder @2047 $ "abc" <> "def"
1182147938584434692
>>> runRollingHashBuilder @2047 "abcdef"
1182147938584434692
>>> mempty @(RollingHashBuilder 2047)
Affine {getAffine1 = 1, getAffine0 = 0}
-}
newtype RollingHashBuilder b = RHB (Affine (RollingHash b))
  deriving newtype (Eq, Show)
  deriving (Semigroup, Monoid) via Dual (Affine (RollingHash b))

instance (KnownNat b) => IsString (RollingHashBuilder b) where
  fromString = F.foldMap' (singletonRHB . ord)

newtype instance U.MVector s (RollingHashBuilder b)
  = MV_RollingHashBuilder (U.MVector s (Affine (RollingHash b)))
newtype instance U.Vector (RollingHashBuilder b)
  = V_RollingHashBuilder (U.Vector (Affine (RollingHash b)))
deriving newtype instance GM.MVector U.MVector (RollingHashBuilder b)
deriving newtype instance G.Vector U.Vector (RollingHashBuilder b)
instance U.Unbox (RollingHashBuilder b)

runRollingHashBuilder :: RollingHashBuilder b -> RollingHash b
runRollingHashBuilder (RHB (Affine _ h)) = h

{- |
>>> :set -XDataKinds
>>> singletonRHB @2047 123
Affine {getAffine1 = 2047, getAffine0 = 123}
-}
singletonRHB :: forall b. (KnownNat b) => Int -> RollingHashBuilder b
singletonRHB x = coerce (Affine (RollingHash b) (RollingHash x))
  where
    b = fromIntegral (natVal' @b proxy#)
