{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Monoid.Affine where

import Control.Monad
import Data.Bits
import qualified Data.Foldable as F
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

-- | a x + b
data Affine a = Affine
  { getAffine1 :: !a
  , getAffine0 :: !a
  }
  deriving (Eq, Show)

instance (Num a) => Semigroup (Affine a) where
  (Affine a0 b0) <> (Affine a1 b1) = Affine (a0 * a1) (a0 * b1 + b0)

instance (Num a) => Monoid (Affine a) where
  mempty = Affine 1 0
  mconcat = F.foldl' (<>) mempty

newtype instance UM.MVector s (Affine a) = MV_Affine (UM.MVector s a)
newtype instance U.Vector (Affine a) = V_Affine (U.Vector a)

instance (U.Unbox a) => U.Unbox (Affine a)

instance (U.Unbox a) => GM.MVector UM.MVector (Affine a) where
  basicLength (MV_Affine v) = unsafeShiftR (GM.basicLength v) 1
  {-# INLINE basicLength #-}
  basicUnsafeSlice i n (MV_Affine v) = MV_Affine $ GM.basicUnsafeSlice (2 * i) (2 * n) v
  {-# INLINE basicUnsafeSlice #-}
  basicOverlaps (MV_Affine v1) (MV_Affine v2) = GM.basicOverlaps v1 v2
  {-# INLINE basicOverlaps #-}
  basicUnsafeNew n = MV_Affine `liftM` GM.basicUnsafeNew (2 * n)
  {-# INLINE basicUnsafeNew #-}
  basicInitialize (MV_Affine v) = GM.basicInitialize v
  {-# INLINE basicInitialize #-}
  basicUnsafeRead (MV_Affine v) i = liftM2 Affine (GM.basicUnsafeRead v (2 * i)) (GM.basicUnsafeRead v (2 * i + 1))
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeWrite (MV_Affine v) i (Affine x y) = GM.basicUnsafeWrite v (2 * i) x >> GM.basicUnsafeWrite v (2 * i + 1) y
  {-# INLINE basicUnsafeWrite #-}
  basicClear (MV_Affine v) = GM.basicClear v
  {-# INLINE basicClear #-}
  basicUnsafeCopy (MV_Affine v1) (MV_Affine v2) = GM.basicUnsafeCopy v1 v2
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeMove (MV_Affine v1) (MV_Affine v2) = GM.basicUnsafeMove v1 v2
  {-# INLINE basicUnsafeMove #-}
  basicUnsafeGrow (MV_Affine v) n = MV_Affine `liftM` GM.basicUnsafeGrow v (2 * n)
  {-# INLINE basicUnsafeGrow #-}

instance (U.Unbox a) => G.Vector U.Vector (Affine a) where
  basicUnsafeFreeze (MV_Affine v) = V_Affine `liftM` G.basicUnsafeFreeze v
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw (V_Affine v) = MV_Affine `liftM` G.basicUnsafeThaw v
  {-# INLINE basicUnsafeThaw #-}
  basicLength (V_Affine v) = unsafeShiftR (G.basicLength v) 1
  {-# INLINE basicLength #-}
  basicUnsafeSlice i n (V_Affine v) = V_Affine $ G.basicUnsafeSlice (2 * i) (2 * n) v
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM (V_Affine v) i = liftM2 Affine (G.basicUnsafeIndexM v (2 * i)) (G.basicUnsafeIndexM v (2 * i + 1))
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeCopy (MV_Affine mv) (V_Affine v) = G.basicUnsafeCopy mv v
  elemseq _ = seq
  {-# INLINE elemseq #-}
