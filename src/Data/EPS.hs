{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Data.EPS where

import Control.Monad
import Data.Coerce
import Data.Semigroup
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

eps :: (Fractional a) => a
eps = 1e-8
{-# INLINE eps #-}

absErr :: Double -> Double -> Double
absErr ans x = abs (x - ans)

relErr :: Double -> Double -> Double
relErr ans x = abs $ (x - ans) / ans

newtype EPS a = EPS {getEPS :: a}
  deriving newtype (Show, Read, Num, Fractional, Floating)

instance (Num a, Ord a, Fractional a) => Eq (EPS a) where
  {-# SPECIALIZE instance Eq (EPS Double) #-}
  (EPS x) == (EPS y) = abs (y - x) < eps

instance (Num a, Ord a, Fractional a) => Ord (EPS a) where
  {-# SPECIALIZE instance Ord (EPS Double) #-}
  compare (EPS x) (EPS y)
    | abs (x - y) < eps = EQ
    | otherwise = compare x y
  (EPS x) < (EPS y) = x < y - eps
  (EPS x) <= (EPS y) = x < y + eps
  (EPS x) > (EPS y) = x > y + eps
  (EPS x) >= (EPS y) = x > y - eps

newtype instance UM.MVector s (EPS a) = MV_EPS (UM.MVector s a)
newtype instance U.Vector (EPS a) = V_EPS (U.Vector a)

instance (U.Unbox a) => U.Unbox (EPS a)

instance (U.Unbox a) => GM.MVector UM.MVector (EPS a) where
  basicLength (MV_EPS v) = GM.basicLength v
  {-# INLINE basicLength #-}
  basicUnsafeSlice i n (MV_EPS v) = MV_EPS $ GM.basicUnsafeSlice i n v
  {-# INLINE basicUnsafeSlice #-}
  basicOverlaps (MV_EPS v1) (MV_EPS v2) = GM.basicOverlaps v1 v2
  {-# INLINE basicOverlaps #-}
  basicUnsafeNew n = MV_EPS `liftM` GM.basicUnsafeNew n
  {-# INLINE basicUnsafeNew #-}
  basicInitialize (MV_EPS v) = GM.basicInitialize v
  {-# INLINE basicInitialize #-}
  basicUnsafeReplicate n x = MV_EPS `liftM` GM.basicUnsafeReplicate n (coerce x)
  {-# INLINE basicUnsafeReplicate #-}
  basicUnsafeRead (MV_EPS v) i = coerce `liftM` GM.basicUnsafeRead v i
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeWrite (MV_EPS v) i x = GM.basicUnsafeWrite v i (coerce x)
  {-# INLINE basicUnsafeWrite #-}
  basicClear (MV_EPS v) = GM.basicClear v
  {-# INLINE basicClear #-}
  basicSet (MV_EPS v) x = GM.basicSet v (coerce x)
  {-# INLINE basicSet #-}
  basicUnsafeCopy (MV_EPS v1) (MV_EPS v2) = GM.basicUnsafeCopy v1 v2
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeMove (MV_EPS v1) (MV_EPS v2) = GM.basicUnsafeMove v1 v2
  {-# INLINE basicUnsafeMove #-}
  basicUnsafeGrow (MV_EPS v) n = MV_EPS `liftM` GM.basicUnsafeGrow v n
  {-# INLINE basicUnsafeGrow #-}

instance (U.Unbox a) => G.Vector U.Vector (EPS a) where
  basicUnsafeFreeze (MV_EPS v) = V_EPS `liftM` G.basicUnsafeFreeze v
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw (V_EPS v) = MV_EPS `liftM` G.basicUnsafeThaw v
  {-# INLINE basicUnsafeThaw #-}
  basicLength (V_EPS v) = G.basicLength v
  {-# INLINE basicLength #-}
  basicUnsafeSlice i n (V_EPS v) = V_EPS $ G.basicUnsafeSlice i n v
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM (V_EPS v) i = coerce `liftM` G.basicUnsafeIndexM v i
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeCopy (MV_EPS mv) (V_EPS v) = G.basicUnsafeCopy mv v
  elemseq _ = seq
  {-# INLINE elemseq #-}
