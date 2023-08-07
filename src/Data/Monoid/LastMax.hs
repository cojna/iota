{-# LANGUAGE TypeFamilies #-}

module Data.Monoid.LastMax where

import Control.Monad
import Data.Coerce
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

newtype LastMax a = LastMax a
  deriving (Eq, Show, Bounded)

instance (Eq a, Bounded a) => Semigroup (LastMax a) where
  x <> y
    | y == minBound = x
    | otherwise = y

instance (Eq a, Bounded a) => Monoid (LastMax a) where
  mempty = minBound
  mconcat = last . (mempty :)

newtype instance UM.MVector s (LastMax a) = MV_LastMax (UM.MVector s a)
newtype instance U.Vector (LastMax a) = V_LastMax (U.Vector a)
instance (U.Unbox a) => U.Unbox (LastMax a)
instance (U.Unbox a) => GM.MVector UM.MVector (LastMax a) where
  basicLength (MV_LastMax v) = GM.basicLength v
  {-# INLINE basicLength #-}
  basicUnsafeSlice i n (MV_LastMax v) = MV_LastMax $ GM.basicUnsafeSlice i n v
  {-# INLINE basicUnsafeSlice #-}
  basicOverlaps (MV_LastMax v1) (MV_LastMax v2) = GM.basicOverlaps v1 v2
  {-# INLINE basicOverlaps #-}
  basicUnsafeNew n = MV_LastMax `liftM` GM.basicUnsafeNew n
  {-# INLINE basicUnsafeNew #-}
  basicInitialize (MV_LastMax v) = GM.basicInitialize v
  {-# INLINE basicInitialize #-}
  basicUnsafeReplicate n x = MV_LastMax `liftM` GM.basicUnsafeReplicate n (coerce x)
  {-# INLINE basicUnsafeReplicate #-}
  basicUnsafeRead (MV_LastMax v) i = coerce `liftM` GM.basicUnsafeRead v i
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeWrite (MV_LastMax v) i x = GM.basicUnsafeWrite v i (coerce x)
  {-# INLINE basicUnsafeWrite #-}
  basicClear (MV_LastMax v) = GM.basicClear v
  {-# INLINE basicClear #-}
  basicSet (MV_LastMax v) x = GM.basicSet v (coerce x)
  {-# INLINE basicSet #-}
  basicUnsafeCopy (MV_LastMax v1) (MV_LastMax v2) = GM.basicUnsafeCopy v1 v2
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeMove (MV_LastMax v1) (MV_LastMax v2) = GM.basicUnsafeMove v1 v2
  {-# INLINE basicUnsafeMove #-}
  basicUnsafeGrow (MV_LastMax v) n = MV_LastMax `liftM` GM.basicUnsafeGrow v n
  {-# INLINE basicUnsafeGrow #-}

instance (U.Unbox a) => G.Vector U.Vector (LastMax a) where
  basicUnsafeFreeze (MV_LastMax v) = V_LastMax `liftM` G.basicUnsafeFreeze v
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw (V_LastMax v) = MV_LastMax `liftM` G.basicUnsafeThaw v
  {-# INLINE basicUnsafeThaw #-}
  basicLength (V_LastMax v) = G.basicLength v
  {-# INLINE basicLength #-}
  basicUnsafeSlice i n (V_LastMax v) = V_LastMax $ G.basicUnsafeSlice i n v
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM (V_LastMax v) i = coerce `liftM` G.basicUnsafeIndexM v i
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeCopy (MV_LastMax mv) (V_LastMax v) = G.basicUnsafeCopy mv v
  elemseq _ = seq
  {-# INLINE elemseq #-}
