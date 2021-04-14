{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.GaloisField where

import Control.Monad
import Data.Coerce
import Data.Proxy
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import GHC.Exts
import GHC.TypeLits

newtype GF (p :: Nat) = GF {unGF :: Int}
  deriving (Eq)
  deriving newtype (Read, Show)

pattern GF# :: Int# -> GF p
pattern GF# x# = GF (I# x#)
{-# COMPLETE GF# #-}

mkGF :: forall p. KnownNat p => Int -> GF p
mkGF x = GF (x `mod` natValAsInt (Proxy @p))

validateGF :: forall p. KnownNat p => GF p -> Bool
validateGF (GF x) = 0 <= x && x < natValAsInt (Proxy @p)

natValAsInt :: KnownNat n => proxy n -> Int
natValAsInt = fromIntegral . natVal
{-# INLINE natValAsInt #-}

natValAsWord :: KnownNat n => proxy n -> Word
natValAsWord = fromIntegral . natVal
{-# INLINE natValAsWord #-}

{- |
>>> reifyNat (998244353 :: Int) natValAsInt
998244353
-}
reifyNat :: (Integral i) => i -> (forall n. KnownNat n => Proxy n -> a) -> a
reifyNat n f = case someNatVal (fromIntegral n) of
  Just (SomeNat proxy) -> f proxy
  Nothing -> error "reifyNat failed"
{-# INLINE reifyNat #-}

asGFOf :: GF p -> Proxy p -> GF p
asGFOf = const
{-# INLINE asGFOf #-}

instance (KnownNat p) => Bounded (GF p) where
  minBound = GF 0
  maxBound = GF (natValAsInt (Proxy @p) - 1)

instance (KnownNat p) => Num (GF p) where
  (GF# x#) + (GF# y#) = case x# +# y# of
    xy# -> GF# (xy# -# ((xy# >=# m#) *# m#))
    where
      !(I# m#) = natValAsInt (Proxy @p)
  {-# INLINE (+) #-}
  (GF# x#) - (GF# y#) = case x# -# y# of
    xy# -> GF# (xy# +# ((xy# <# 0#) *# m#))
    where
      !(I# m#) = natValAsInt (Proxy @p)
  {-# INLINE (-) #-}
  (GF# x#) * (GF# y#) = case timesWord# (int2Word# x#) (int2Word# y#) of
    z# -> case timesWord2# z# im# of
      (# q#, _ #) -> case minusWord# z# (timesWord# q# m#) of
        v#
          | isTrue# (geWord# v# m#) -> GF# (word2Int# (plusWord# v# m#))
          | otherwise -> GF# (word2Int# v#)
    where
      !(W# m#) = natValAsWord (Proxy @p)
      im# = plusWord# (quotWord# 0xffffffffffffffff## m#) 1##
  {-# INLINE (*) #-}
  abs = id
  signum = const (GF 1)
  fromInteger x = GF . fromIntegral $ x `mod` m
    where
      m = natVal (Proxy @p)

instance (KnownNat p) => Fractional (GF p) where
  (GF# x#) / (GF# y#) = go# y# m# 1# 0#
    where
      !(I# m#) = natValAsInt (Proxy @p)
      go# a# b# u# v#
        | isTrue# (b# ># 0#) = case a# `quotInt#` b# of
          q# -> go# b# (a# -# (q# *# b#)) v# (u# -# (q# *# v#))
        | otherwise = GF# (x# *# (u# +# m#) `remInt#` m#)
  fromRational _ = undefined

newtype instance UM.MVector s (GF p) = MV_GF (UM.MVector s Int)
newtype instance U.Vector (GF p) = V_GF (U.Vector Int)

instance U.Unbox (GF p)

instance GM.MVector UM.MVector (GF p) where
  basicLength (MV_GF v) = GM.basicLength v
  {-# INLINE basicLength #-}
  basicUnsafeSlice i n (MV_GF v) = MV_GF $ GM.basicUnsafeSlice i n v
  {-# INLINE basicUnsafeSlice #-}
  basicOverlaps (MV_GF v1) (MV_GF v2) = GM.basicOverlaps v1 v2
  {-# INLINE basicOverlaps #-}
  basicUnsafeNew n = MV_GF `liftM` GM.basicUnsafeNew n
  {-# INLINE basicUnsafeNew #-}
  basicInitialize (MV_GF v) = GM.basicInitialize v
  {-# INLINE basicInitialize #-}
  basicUnsafeReplicate n x = MV_GF `liftM` GM.basicUnsafeReplicate n (coerce x)
  {-# INLINE basicUnsafeReplicate #-}
  basicUnsafeRead (MV_GF v) i = coerce `liftM` GM.basicUnsafeRead v i
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeWrite (MV_GF v) i x = GM.basicUnsafeWrite v i (coerce x)
  {-# INLINE basicUnsafeWrite #-}
  basicClear (MV_GF v) = GM.basicClear v
  {-# INLINE basicClear #-}
  basicSet (MV_GF v) x = GM.basicSet v (coerce x)
  {-# INLINE basicSet #-}
  basicUnsafeCopy (MV_GF v1) (MV_GF v2) = GM.basicUnsafeCopy v1 v2
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeMove (MV_GF v1) (MV_GF v2) = GM.basicUnsafeMove v1 v2
  {-# INLINE basicUnsafeMove #-}
  basicUnsafeGrow (MV_GF v) n = MV_GF `liftM` GM.basicUnsafeGrow v n
  {-# INLINE basicUnsafeGrow #-}

instance G.Vector U.Vector (GF p) where
  basicUnsafeFreeze (MV_GF v) = V_GF `liftM` G.basicUnsafeFreeze v
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw (V_GF v) = MV_GF `liftM` G.basicUnsafeThaw v
  {-# INLINE basicUnsafeThaw #-}
  basicLength (V_GF v) = G.basicLength v
  {-# INLINE basicLength #-}
  basicUnsafeSlice i n (V_GF v) = V_GF $ G.basicUnsafeSlice i n v
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM (V_GF v) i = coerce `liftM` G.basicUnsafeIndexM v i
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeCopy (MV_GF mv) (V_GF v) = G.basicUnsafeCopy mv v
  elemseq _ = seq
  {-# INLINE elemseq #-}
