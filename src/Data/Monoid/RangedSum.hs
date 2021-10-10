{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Monoid.RangedSum where

import Control.Monad
import Data.Bits
import qualified Data.Foldable as F
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

data RangedSum a = RangedSum
  { getRangesSumSize :: !a
  , getRangedSum :: !a
  }
  deriving (Eq, Show)

instance (Num a) => Semigroup (RangedSum a) where
  (RangedSum len0 x) <> (RangedSum len1 y) = RangedSum (len0 + len1) (x + y)

instance (Num a) => Monoid (RangedSum a) where
  mempty = RangedSum 0 0
  mconcat = F.foldl' (<>) mempty

newtype instance UM.MVector s (RangedSum a) = MV_RangedSum (UM.MVector s a)
newtype instance U.Vector (RangedSum a) = V_RangedSum (U.Vector a)

instance (U.Unbox a) => U.Unbox (RangedSum a)

instance (U.Unbox a) => GM.MVector UM.MVector (RangedSum a) where
  basicLength (MV_RangedSum v) = unsafeShiftR (GM.basicLength v) 1
  {-# INLINE basicLength #-}
  basicUnsafeSlice i n (MV_RangedSum v) = MV_RangedSum $ GM.basicUnsafeSlice (2 * i) (2 * n) v
  {-# INLINE basicUnsafeSlice #-}
  basicOverlaps (MV_RangedSum v1) (MV_RangedSum v2) = GM.basicOverlaps v1 v2
  {-# INLINE basicOverlaps #-}
  basicUnsafeNew n = MV_RangedSum `liftM` GM.basicUnsafeNew (2 * n)
  {-# INLINE basicUnsafeNew #-}
  basicInitialize (MV_RangedSum v) = GM.basicInitialize v
  {-# INLINE basicInitialize #-}
  basicUnsafeRead (MV_RangedSum v) i = liftM2 RangedSum (GM.basicUnsafeRead v (2 * i)) (GM.basicUnsafeRead v (2 * i + 1))
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeWrite (MV_RangedSum v) i (RangedSum x y) = GM.basicUnsafeWrite v (2 * i) x >> GM.basicUnsafeWrite v (2 * i + 1) y
  {-# INLINE basicUnsafeWrite #-}
  basicClear (MV_RangedSum v) = GM.basicClear v
  {-# INLINE basicClear #-}
  basicUnsafeCopy (MV_RangedSum v1) (MV_RangedSum v2) = GM.basicUnsafeCopy v1 v2
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeMove (MV_RangedSum v1) (MV_RangedSum v2) = GM.basicUnsafeMove v1 v2
  {-# INLINE basicUnsafeMove #-}
  basicUnsafeGrow (MV_RangedSum v) n = MV_RangedSum `liftM` GM.basicUnsafeGrow v (2 * n)
  {-# INLINE basicUnsafeGrow #-}

instance (U.Unbox a) => G.Vector U.Vector (RangedSum a) where
  basicUnsafeFreeze (MV_RangedSum v) = V_RangedSum `liftM` G.basicUnsafeFreeze v
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw (V_RangedSum v) = MV_RangedSum `liftM` G.basicUnsafeThaw v
  {-# INLINE basicUnsafeThaw #-}
  basicLength (V_RangedSum v) = unsafeShiftR (G.basicLength v) 1
  {-# INLINE basicLength #-}
  basicUnsafeSlice i n (V_RangedSum v) = V_RangedSum $ G.basicUnsafeSlice (2 * i) (2 * n) v
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM (V_RangedSum v) i = liftM2 RangedSum (G.basicUnsafeIndexM v (2 * i)) (G.basicUnsafeIndexM v (2 * i + 1))
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeCopy (MV_RangedSum mv) (V_RangedSum v) = G.basicUnsafeCopy mv v
  elemseq _ = seq
  {-# INLINE elemseq #-}
