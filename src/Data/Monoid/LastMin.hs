{-# LANGUAGE TypeFamilies #-}

module Data.Monoid.LastMin where

import Control.Monad
import Data.Coerce
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

newtype LastMin a = LastMin a
  deriving (Eq, Show, Bounded)

instance (Eq a, Bounded a) => Semigroup (LastMin a) where
  x <> y
    | y == maxBound = x
    | otherwise = y

instance (Eq a, Bounded a) => Monoid (LastMin a) where
  mempty = maxBound
  mconcat = last . (mempty :)

newtype instance UM.MVector s (LastMin a) = MV_LastMin (UM.MVector s a)
newtype instance U.Vector (LastMin a) = V_LastMin (U.Vector a)
instance (U.Unbox a) => U.Unbox (LastMin a)
instance (U.Unbox a) => GM.MVector UM.MVector (LastMin a) where
  basicLength (MV_LastMin v) = GM.basicLength v
  {-# INLINE basicLength #-}
  basicUnsafeSlice i n (MV_LastMin v) = MV_LastMin $ GM.basicUnsafeSlice i n v
  {-# INLINE basicUnsafeSlice #-}
  basicOverlaps (MV_LastMin v1) (MV_LastMin v2) = GM.basicOverlaps v1 v2
  {-# INLINE basicOverlaps #-}
  basicUnsafeNew n = MV_LastMin `liftM` GM.basicUnsafeNew n
  {-# INLINE basicUnsafeNew #-}
  basicInitialize (MV_LastMin v) = GM.basicInitialize v
  {-# INLINE basicInitialize #-}
  basicUnsafeReplicate n x = MV_LastMin `liftM` GM.basicUnsafeReplicate n (coerce x)
  {-# INLINE basicUnsafeReplicate #-}
  basicUnsafeRead (MV_LastMin v) i = coerce `liftM` GM.basicUnsafeRead v i
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeWrite (MV_LastMin v) i x = GM.basicUnsafeWrite v i (coerce x)
  {-# INLINE basicUnsafeWrite #-}
  basicClear (MV_LastMin v) = GM.basicClear v
  {-# INLINE basicClear #-}
  basicSet (MV_LastMin v) x = GM.basicSet v (coerce x)
  {-# INLINE basicSet #-}
  basicUnsafeCopy (MV_LastMin v1) (MV_LastMin v2) = GM.basicUnsafeCopy v1 v2
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeMove (MV_LastMin v1) (MV_LastMin v2) = GM.basicUnsafeMove v1 v2
  {-# INLINE basicUnsafeMove #-}
  basicUnsafeGrow (MV_LastMin v) n = MV_LastMin `liftM` GM.basicUnsafeGrow v n
  {-# INLINE basicUnsafeGrow #-}

instance (U.Unbox a) => G.Vector U.Vector (LastMin a) where
  basicUnsafeFreeze (MV_LastMin v) = V_LastMin `liftM` G.basicUnsafeFreeze v
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw (V_LastMin v) = MV_LastMin `liftM` G.basicUnsafeThaw v
  {-# INLINE basicUnsafeThaw #-}
  basicLength (V_LastMin v) = G.basicLength v
  {-# INLINE basicLength #-}
  basicUnsafeSlice i n (V_LastMin v) = V_LastMin $ G.basicUnsafeSlice i n v
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM (V_LastMin v) i = coerce `liftM` G.basicUnsafeIndexM v i
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeCopy (MV_LastMin mv) (V_LastMin v) = G.basicUnsafeCopy mv v
  elemseq _ = seq
  {-# INLINE elemseq #-}
