{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Monoid.BitAnd where

import Control.Monad
import Data.Bits
import Data.Coerce (coerce)
import qualified Data.Foldable as F
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

{- |
>>> mempty :: BitAnd Int
BitAnd {getBitAnd = -1}
>>> mempty :: BitAnd Word
BitAnd {getBitAnd = 18446744073709551615}
-}
newtype BitAnd a = BitAnd {getBitAnd :: a}
  deriving (Eq, Show)

instance (Bits a) => Semigroup (BitAnd a) where
  (<>) = coerce ((.&.) @a)
  {-# INLINE (<>) #-}

instance (Bits a) => Monoid (BitAnd a) where
  mempty = coerce $ complement (zeroBits @a)
  {-# INLINE mempty #-}
  mconcat = F.foldl' mappend mempty
  {-# INLINE mconcat #-}

newtype instance UM.MVector s (BitAnd a) = MV_BitAnd (UM.MVector s a)
newtype instance U.Vector (BitAnd a) = V_BitAnd (U.Vector a)
instance (U.Unbox a) => U.Unbox (BitAnd a)
instance (U.Unbox a) => GM.MVector UM.MVector (BitAnd a) where
  basicLength (MV_BitAnd v) = GM.basicLength v
  {-# INLINE basicLength #-}
  basicUnsafeSlice i n (MV_BitAnd v) = MV_BitAnd $ GM.basicUnsafeSlice i n v
  {-# INLINE basicUnsafeSlice #-}
  basicOverlaps (MV_BitAnd v1) (MV_BitAnd v2) = GM.basicOverlaps v1 v2
  {-# INLINE basicOverlaps #-}
  basicUnsafeNew n = MV_BitAnd `liftM` GM.basicUnsafeNew n
  {-# INLINE basicUnsafeNew #-}
  basicInitialize (MV_BitAnd v) = GM.basicInitialize v
  {-# INLINE basicInitialize #-}
  basicUnsafeReplicate n x = MV_BitAnd `liftM` GM.basicUnsafeReplicate n (coerce x)
  {-# INLINE basicUnsafeReplicate #-}
  basicUnsafeRead (MV_BitAnd v) i = coerce `liftM` GM.basicUnsafeRead v i
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeWrite (MV_BitAnd v) i x = GM.basicUnsafeWrite v i (coerce x)
  {-# INLINE basicUnsafeWrite #-}
  basicClear (MV_BitAnd v) = GM.basicClear v
  {-# INLINE basicClear #-}
  basicSet (MV_BitAnd v) x = GM.basicSet v (coerce x)
  {-# INLINE basicSet #-}
  basicUnsafeCopy (MV_BitAnd v1) (MV_BitAnd v2) = GM.basicUnsafeCopy v1 v2
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeMove (MV_BitAnd v1) (MV_BitAnd v2) = GM.basicUnsafeMove v1 v2
  {-# INLINE basicUnsafeMove #-}
  basicUnsafeGrow (MV_BitAnd v) n = MV_BitAnd `liftM` GM.basicUnsafeGrow v n
  {-# INLINE basicUnsafeGrow #-}

instance (U.Unbox a) => G.Vector U.Vector (BitAnd a) where
  basicUnsafeFreeze (MV_BitAnd v) = V_BitAnd `liftM` G.basicUnsafeFreeze v
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw (V_BitAnd v) = MV_BitAnd `liftM` G.basicUnsafeThaw v
  {-# INLINE basicUnsafeThaw #-}
  basicLength (V_BitAnd v) = G.basicLength v
  {-# INLINE basicLength #-}
  basicUnsafeSlice i n (V_BitAnd v) = V_BitAnd $ G.basicUnsafeSlice i n v
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM (V_BitAnd v) i = coerce `liftM` G.basicUnsafeIndexM v i
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeCopy (MV_BitAnd mv) (V_BitAnd v) = G.basicUnsafeCopy mv v
  elemseq _ = seq
  {-# INLINE elemseq #-}
