{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Monoid.LCM where

import Control.Monad
import Data.Coerce (coerce)
import qualified Data.Foldable as F
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

{- |
>>> mempty :: LCM Int
LCM {getLCM = 1}
>>> LCM (-2) <> LCM 3
LCM {getLCM = 6}
>>> LCM (-1) <> mempty
LCM {getLCM = 1}
-}
newtype LCM a = LCM {getLCM :: a}
  deriving (Eq, Show)

instance (Integral a) => Semigroup (LCM a) where
  (<>) = coerce (lcm @a)
  {-# INLINE (<>) #-}

instance (Num a, Integral a) => Monoid (LCM a) where
  mempty = LCM 1
  {-# INLINE mempty #-}
  mconcat = F.foldl' mappend mempty
  {-# INLINE mconcat #-}

newtype instance UM.MVector s (LCM a) = MV_LCM (UM.MVector s a)
newtype instance U.Vector (LCM a) = V_LCM (U.Vector a)
instance (U.Unbox a) => U.Unbox (LCM a)
instance (U.Unbox a) => GM.MVector UM.MVector (LCM a) where
  basicLength (MV_LCM v) = GM.basicLength v
  {-# INLINE basicLength #-}
  basicUnsafeSlice i n (MV_LCM v) = MV_LCM $ GM.basicUnsafeSlice i n v
  {-# INLINE basicUnsafeSlice #-}
  basicOverlaps (MV_LCM v1) (MV_LCM v2) = GM.basicOverlaps v1 v2
  {-# INLINE basicOverlaps #-}
  basicUnsafeNew n = MV_LCM `liftM` GM.basicUnsafeNew n
  {-# INLINE basicUnsafeNew #-}
  basicInitialize (MV_LCM v) = GM.basicInitialize v
  {-# INLINE basicInitialize #-}
  basicUnsafeReplicate n x = MV_LCM `liftM` GM.basicUnsafeReplicate n (coerce x)
  {-# INLINE basicUnsafeReplicate #-}
  basicUnsafeRead (MV_LCM v) i = coerce `liftM` GM.basicUnsafeRead v i
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeWrite (MV_LCM v) i x = GM.basicUnsafeWrite v i (coerce x)
  {-# INLINE basicUnsafeWrite #-}
  basicClear (MV_LCM v) = GM.basicClear v
  {-# INLINE basicClear #-}
  basicSet (MV_LCM v) x = GM.basicSet v (coerce x)
  {-# INLINE basicSet #-}
  basicUnsafeCopy (MV_LCM v1) (MV_LCM v2) = GM.basicUnsafeCopy v1 v2
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeMove (MV_LCM v1) (MV_LCM v2) = GM.basicUnsafeMove v1 v2
  {-# INLINE basicUnsafeMove #-}
  basicUnsafeGrow (MV_LCM v) n = MV_LCM `liftM` GM.basicUnsafeGrow v n
  {-# INLINE basicUnsafeGrow #-}

instance (U.Unbox a) => G.Vector U.Vector (LCM a) where
  basicUnsafeFreeze (MV_LCM v) = V_LCM `liftM` G.basicUnsafeFreeze v
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw (V_LCM v) = MV_LCM `liftM` G.basicUnsafeThaw v
  {-# INLINE basicUnsafeThaw #-}
  basicLength (V_LCM v) = G.basicLength v
  {-# INLINE basicLength #-}
  basicUnsafeSlice i n (V_LCM v) = V_LCM $ G.basicUnsafeSlice i n v
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM (V_LCM v) i = coerce `liftM` G.basicUnsafeIndexM v i
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeCopy (MV_LCM mv) (V_LCM v) = G.basicUnsafeCopy mv v
  elemseq _ = seq
  {-# INLINE elemseq #-}
