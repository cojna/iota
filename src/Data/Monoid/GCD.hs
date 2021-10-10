{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Monoid.GCD where

import Control.Monad
import Data.Coerce (coerce)
import qualified Data.Foldable as F
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

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

newtype instance UM.MVector s (GCD a) = MV_GCD (UM.MVector s a)
newtype instance U.Vector (GCD a) = V_GCD (U.Vector a)
instance (U.Unbox a) => U.Unbox (GCD a)
instance (U.Unbox a) => GM.MVector UM.MVector (GCD a) where
  basicLength (MV_GCD v) = GM.basicLength v
  {-# INLINE basicLength #-}
  basicUnsafeSlice i n (MV_GCD v) = MV_GCD $ GM.basicUnsafeSlice i n v
  {-# INLINE basicUnsafeSlice #-}
  basicOverlaps (MV_GCD v1) (MV_GCD v2) = GM.basicOverlaps v1 v2
  {-# INLINE basicOverlaps #-}
  basicUnsafeNew n = MV_GCD `liftM` GM.basicUnsafeNew n
  {-# INLINE basicUnsafeNew #-}
  basicInitialize (MV_GCD v) = GM.basicInitialize v
  {-# INLINE basicInitialize #-}
  basicUnsafeReplicate n x = MV_GCD `liftM` GM.basicUnsafeReplicate n (coerce x)
  {-# INLINE basicUnsafeReplicate #-}
  basicUnsafeRead (MV_GCD v) i = coerce `liftM` GM.basicUnsafeRead v i
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeWrite (MV_GCD v) i x = GM.basicUnsafeWrite v i (coerce x)
  {-# INLINE basicUnsafeWrite #-}
  basicClear (MV_GCD v) = GM.basicClear v
  {-# INLINE basicClear #-}
  basicSet (MV_GCD v) x = GM.basicSet v (coerce x)
  {-# INLINE basicSet #-}
  basicUnsafeCopy (MV_GCD v1) (MV_GCD v2) = GM.basicUnsafeCopy v1 v2
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeMove (MV_GCD v1) (MV_GCD v2) = GM.basicUnsafeMove v1 v2
  {-# INLINE basicUnsafeMove #-}
  basicUnsafeGrow (MV_GCD v) n = MV_GCD `liftM` GM.basicUnsafeGrow v n
  {-# INLINE basicUnsafeGrow #-}

instance (U.Unbox a) => G.Vector U.Vector (GCD a) where
  basicUnsafeFreeze (MV_GCD v) = V_GCD `liftM` G.basicUnsafeFreeze v
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw (V_GCD v) = MV_GCD `liftM` G.basicUnsafeThaw v
  {-# INLINE basicUnsafeThaw #-}
  basicLength (V_GCD v) = G.basicLength v
  {-# INLINE basicLength #-}
  basicUnsafeSlice i n (V_GCD v) = V_GCD $ G.basicUnsafeSlice i n v
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM (V_GCD v) i = coerce `liftM` G.basicUnsafeIndexM v i
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeCopy (MV_GCD mv) (V_GCD v) = G.basicUnsafeCopy mv v
  elemseq _ = seq
  {-# INLINE elemseq #-}
