{-# LANGUAGE TypeFamilies #-}

module Data.Monoid.BitOr where

import Control.Monad
import Data.Bits
import Data.Coerce (coerce)
import qualified Data.Foldable as F
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

{- |
>>> mempty :: BitOr Int
BitOr {getBitOr = 0}
-}
newtype BitOr a = BitOr {getBitOr :: a}
  deriving (Eq, Show)

instance (Bits a) => Semigroup (BitOr a) where
  (<>) = coerce ((.|.) @a)
  {-# INLINE (<>) #-}

instance (Bits a) => Monoid (BitOr a) where
  mempty = coerce (zeroBits @a)
  {-# INLINE mempty #-}
  mconcat = F.foldl' mappend mempty
  {-# INLINE mconcat #-}

newtype instance UM.MVector s (BitOr a) = MV_BitOr (UM.MVector s a)
newtype instance U.Vector (BitOr a) = V_BitOr (U.Vector a)
instance (U.Unbox a) => U.Unbox (BitOr a)
instance (U.Unbox a) => GM.MVector UM.MVector (BitOr a) where
  basicLength (MV_BitOr v) = GM.basicLength v
  {-# INLINE basicLength #-}
  basicUnsafeSlice i n (MV_BitOr v) = MV_BitOr $ GM.basicUnsafeSlice i n v
  {-# INLINE basicUnsafeSlice #-}
  basicOverlaps (MV_BitOr v1) (MV_BitOr v2) = GM.basicOverlaps v1 v2
  {-# INLINE basicOverlaps #-}
  basicUnsafeNew n = MV_BitOr `liftM` GM.basicUnsafeNew n
  {-# INLINE basicUnsafeNew #-}
  basicInitialize (MV_BitOr v) = GM.basicInitialize v
  {-# INLINE basicInitialize #-}
  basicUnsafeReplicate n x = MV_BitOr `liftM` GM.basicUnsafeReplicate n (coerce x)
  {-# INLINE basicUnsafeReplicate #-}
  basicUnsafeRead (MV_BitOr v) i = coerce `liftM` GM.basicUnsafeRead v i
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeWrite (MV_BitOr v) i x = GM.basicUnsafeWrite v i (coerce x)
  {-# INLINE basicUnsafeWrite #-}
  basicClear (MV_BitOr v) = GM.basicClear v
  {-# INLINE basicClear #-}
  basicSet (MV_BitOr v) x = GM.basicSet v (coerce x)
  {-# INLINE basicSet #-}
  basicUnsafeCopy (MV_BitOr v1) (MV_BitOr v2) = GM.basicUnsafeCopy v1 v2
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeMove (MV_BitOr v1) (MV_BitOr v2) = GM.basicUnsafeMove v1 v2
  {-# INLINE basicUnsafeMove #-}
  basicUnsafeGrow (MV_BitOr v) n = MV_BitOr `liftM` GM.basicUnsafeGrow v n
  {-# INLINE basicUnsafeGrow #-}

instance (U.Unbox a) => G.Vector U.Vector (BitOr a) where
  basicUnsafeFreeze (MV_BitOr v) = V_BitOr `liftM` G.basicUnsafeFreeze v
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw (V_BitOr v) = MV_BitOr `liftM` G.basicUnsafeThaw v
  {-# INLINE basicUnsafeThaw #-}
  basicLength (V_BitOr v) = G.basicLength v
  {-# INLINE basicLength #-}
  basicUnsafeSlice i n (V_BitOr v) = V_BitOr $ G.basicUnsafeSlice i n v
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM (V_BitOr v) i = coerce `liftM` G.basicUnsafeIndexM v i
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeCopy (MV_BitOr mv) (V_BitOr v) = G.basicUnsafeCopy mv v
  elemseq _ = seq
  {-# INLINE elemseq #-}
