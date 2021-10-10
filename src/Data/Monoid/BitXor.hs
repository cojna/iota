{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Monoid.BitXor where

import Control.Monad
import Data.Bits
import Data.Coerce (coerce)
import qualified Data.Foldable as F
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

{- |
>>> mempty :: BitXor Int
BitXor {getBitXor = 0}
-}
newtype BitXor a = BitXor {getBitXor :: a}
  deriving (Eq, Show)

instance (Bits a) => Semigroup (BitXor a) where
  (<>) = coerce (xor @a)
  {-# INLINE (<>) #-}

instance (Bits a) => Monoid (BitXor a) where
  mempty = coerce (zeroBits @a)
  {-# INLINE mempty #-}
  mconcat = F.foldl' mappend mempty
  {-# INLINE mconcat #-}

newtype instance UM.MVector s (BitXor a) = MV_BitXor (UM.MVector s a)
newtype instance U.Vector (BitXor a) = V_BitXor (U.Vector a)
instance (U.Unbox a) => U.Unbox (BitXor a)
instance (U.Unbox a) => GM.MVector UM.MVector (BitXor a) where
  basicLength (MV_BitXor v) = GM.basicLength v
  {-# INLINE basicLength #-}
  basicUnsafeSlice i n (MV_BitXor v) = MV_BitXor $ GM.basicUnsafeSlice i n v
  {-# INLINE basicUnsafeSlice #-}
  basicOverlaps (MV_BitXor v1) (MV_BitXor v2) = GM.basicOverlaps v1 v2
  {-# INLINE basicOverlaps #-}
  basicUnsafeNew n = MV_BitXor `liftM` GM.basicUnsafeNew n
  {-# INLINE basicUnsafeNew #-}
  basicInitialize (MV_BitXor v) = GM.basicInitialize v
  {-# INLINE basicInitialize #-}
  basicUnsafeReplicate n x = MV_BitXor `liftM` GM.basicUnsafeReplicate n (coerce x)
  {-# INLINE basicUnsafeReplicate #-}
  basicUnsafeRead (MV_BitXor v) i = coerce `liftM` GM.basicUnsafeRead v i
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeWrite (MV_BitXor v) i x = GM.basicUnsafeWrite v i (coerce x)
  {-# INLINE basicUnsafeWrite #-}
  basicClear (MV_BitXor v) = GM.basicClear v
  {-# INLINE basicClear #-}
  basicSet (MV_BitXor v) x = GM.basicSet v (coerce x)
  {-# INLINE basicSet #-}
  basicUnsafeCopy (MV_BitXor v1) (MV_BitXor v2) = GM.basicUnsafeCopy v1 v2
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeMove (MV_BitXor v1) (MV_BitXor v2) = GM.basicUnsafeMove v1 v2
  {-# INLINE basicUnsafeMove #-}
  basicUnsafeGrow (MV_BitXor v) n = MV_BitXor `liftM` GM.basicUnsafeGrow v n
  {-# INLINE basicUnsafeGrow #-}

instance (U.Unbox a) => G.Vector U.Vector (BitXor a) where
  basicUnsafeFreeze (MV_BitXor v) = V_BitXor `liftM` G.basicUnsafeFreeze v
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw (V_BitXor v) = MV_BitXor `liftM` G.basicUnsafeThaw v
  {-# INLINE basicUnsafeThaw #-}
  basicLength (V_BitXor v) = G.basicLength v
  {-# INLINE basicLength #-}
  basicUnsafeSlice i n (V_BitXor v) = V_BitXor $ G.basicUnsafeSlice i n v
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM (V_BitXor v) i = coerce `liftM` G.basicUnsafeIndexM v i
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeCopy (MV_BitXor mv) (V_BitXor v) = G.basicUnsafeCopy mv v
  elemseq _ = seq
  {-# INLINE elemseq #-}
