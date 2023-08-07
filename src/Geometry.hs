{-# LANGUAGE TypeFamilies #-}

module Geometry where

import Control.Monad
import Data.Bits (unsafeShiftR)
import Data.EPS (EPS (..))
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

data Point a = P !a !a deriving (Eq, Ord)

instance (Show a) => Show (Point a) where
  show (P x y) = shows x $ ' ' : show y

instance Functor Point where
  fmap f (P x y) = P (f x) (f y)

instance (Num a) => Num (Point a) where
  {-# SPECIALIZE instance Num (Point Int) #-}
  {-# SPECIALIZE instance Num (Point Integer) #-}
  {-# SPECIALIZE instance Num (Point Double) #-}
  {-# SPECIALIZE instance Num (Point (EPS Double)) #-}
  (P x0 y0) + (P x1 y1) = P (x0 + x1) (y0 + y1)
  (P x0 y0) - (P x1 y1) = P (x0 - x1) (y0 - y1)
  (P x0 y0) * (P x1 y1) = P (x0 * x1 - y0 * y1) (x0 * y1 + x1 * y0)
  negate = fmap negate
  abs = id
  signum _ = P 1 0
  fromInteger n = P (fromInteger n) 0

dot :: (Num a) => Point a -> Point a -> a
dot (P x0 y0) (P x1 y1) = x0 * x1 + y0 * y1
{-# INLINE dot #-}

cross :: (Num a) => Point a -> Point a -> a
cross (P x0 y0) (P x1 y1) = x0 * y1 - y0 * x1
{-# INLINE cross #-}

conjugate :: (Num a) => Point a -> Point a
conjugate (P x y) = P x (-y)
{-# INLINE conjugate #-}

area :: (Num a) => Point a -> Point a -> Point a -> a
area o u v = cross (u - o) (v - o)
{-# INLINE area #-}

compareCCW :: (Num a, Ord a) => Point a -> Point a -> Point a -> Ordering
compareCCW o = \u v -> compare 0 (area o u v)
{-# INLINE compareCCW #-}

compareCW :: (Num a, Ord a) => Point a -> Point a -> Point a -> Ordering
compareCW o = flip (compareCCW o)
{-# INLINE compareCW #-}

sqrNorm :: (Num a) => Point a -> a
sqrNorm v = dot v v
{-# INLINE sqrNorm #-}

norm :: (Floating a) => Point a -> a
norm = sqrt . sqrNorm
{-# INLINE norm #-}

newtype instance UM.MVector s (Point a) = MV_Point (UM.MVector s a)
newtype instance U.Vector (Point a) = V_Point (U.Vector a)

instance (U.Unbox a) => U.Unbox (Point a)

instance (U.Unbox a) => GM.MVector UM.MVector (Point a) where
  basicLength (MV_Point v) = unsafeShiftR (GM.basicLength v) 1
  {-# INLINE basicLength #-}
  basicUnsafeSlice i n (MV_Point v) = MV_Point $ GM.basicUnsafeSlice (2 * i) (2 * n) v
  {-# INLINE basicUnsafeSlice #-}
  basicOverlaps (MV_Point v1) (MV_Point v2) = GM.basicOverlaps v1 v2
  {-# INLINE basicOverlaps #-}
  basicUnsafeNew n = MV_Point `liftM` GM.basicUnsafeNew (2 * n)
  {-# INLINE basicUnsafeNew #-}
  basicInitialize (MV_Point v) = GM.basicInitialize v
  {-# INLINE basicInitialize #-}
  basicUnsafeRead (MV_Point v) i = P `liftM` GM.basicUnsafeRead v (2 * i) `ap` GM.basicUnsafeRead v (2 * i + 1)
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeWrite (MV_Point v) i (P x y) = GM.basicUnsafeWrite v (2 * i) x >> GM.basicUnsafeWrite v (2 * i + 1) y
  {-# INLINE basicUnsafeWrite #-}
  basicClear (MV_Point v) = GM.basicClear v
  {-# INLINE basicClear #-}
  basicUnsafeCopy (MV_Point v1) (MV_Point v2) = GM.basicUnsafeCopy v1 v2
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeMove (MV_Point v1) (MV_Point v2) = GM.basicUnsafeMove v1 v2
  {-# INLINE basicUnsafeMove #-}
  basicUnsafeGrow (MV_Point v) n = MV_Point `liftM` GM.basicUnsafeGrow v (2 * n)
  {-# INLINE basicUnsafeGrow #-}

instance (U.Unbox a) => G.Vector U.Vector (Point a) where
  basicUnsafeFreeze (MV_Point v) = V_Point `liftM` G.basicUnsafeFreeze v
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw (V_Point v) = MV_Point `liftM` G.basicUnsafeThaw v
  {-# INLINE basicUnsafeThaw #-}
  basicLength (V_Point v) = unsafeShiftR (G.basicLength v) 1
  {-# INLINE basicLength #-}
  basicUnsafeSlice i n (V_Point v) = V_Point $ G.basicUnsafeSlice (2 * i) (2 * n) v
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM (V_Point v) i = P `liftM` G.basicUnsafeIndexM v (2 * i) `ap` G.basicUnsafeIndexM v (2 * i + 1)
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeCopy (MV_Point mv) (V_Point v) = G.basicUnsafeCopy mv v
  elemseq _ = seq
  {-# INLINE elemseq #-}
