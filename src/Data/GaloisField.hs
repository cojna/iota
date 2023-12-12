{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.GaloisField where

import Data.Int
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

mkGF :: forall p. (KnownNat p) => Int -> GF p
mkGF x = GF (x `mod` fromIntegral (natVal' (proxy# @p)))

validateGF :: forall p. (KnownNat p) => GF p -> Bool
validateGF (GF x) = 0 <= x && x < natValAsInt (Proxy @p)

natValAsInt :: (KnownNat n) => proxy n -> Int
natValAsInt = fromIntegral . natVal
{-# INLINE natValAsInt #-}

natValAsWord :: (KnownNat n) => proxy n -> Word
natValAsWord = fromIntegral . natVal
{-# INLINE natValAsWord #-}

{- |
>>> reifyNat (998244353 :: Int) natValAsInt
998244353
-}
reifyNat :: (Integral i) => i -> (forall n. (KnownNat n) => Proxy n -> a) -> a
reifyNat n f = case someNatVal (fromIntegral n) of
  Just (SomeNat proxy) -> f proxy
  Nothing -> error "reifyNat failed"
{-# INLINE reifyNat #-}

asGFOf :: GF p -> Proxy p -> GF p
asGFOf = const
{-# INLINE asGFOf #-}

instance (KnownNat p) => Bounded (GF p) where
  minBound = GF 0
  maxBound = GF (fromIntegral (natVal' (proxy# @p)) - 1)

instance (KnownNat p) => Num (GF p) where
  (GF# x#) + (GF# y#) = case x# +# y# of
    xy# -> GF# (xy# -# ((xy# >=# m#) *# m#))
    where
      !(I# m#) = fromIntegral $ natVal' (proxy# @p)
  {-# INLINE (+) #-}
  (GF# x#) - (GF# y#) = case x# -# y# of
    xy# -> GF# (xy# +# ((xy# <# 0#) *# m#))
    where
      !(I# m#) = fromIntegral $ natVal' (proxy# @p)
  {-# INLINE (-) #-}
  (GF# x#) * (GF# y#) = case timesWord# (int2Word# x#) (int2Word# y#) of
    z# -> case timesWord2# z# im# of
      (# q#, _ #) -> case minusWord# z# (timesWord# q# m#) of
        v#
          | isTrue# (geWord# v# m#) -> GF# (word2Int# (plusWord# v# m#))
          | otherwise -> GF# (word2Int# v#)
    where
      !(W# m#) = fromIntegral $ natVal' (proxy# @p)
      im# = plusWord# (quotWord# 0xffffffffffffffff## m#) 1##
  {-# INLINE (*) #-}
  abs = id
  signum = const (GF 1)
  fromInteger x = GF . fromIntegral $ x `mod` m
    where
      m = natVal' (proxy# @p)

instance (KnownNat p) => Fractional (GF p) where
  (GF# x#) / (GF# y#) = go# y# m# 1# 0#
    where
      !(I# m#) = fromIntegral $ natVal' (proxy# @p)
      go# a# b# u# v#
        | isTrue# (b# ># 0#) = case a# `quotInt#` b# of
            q# -> go# b# (a# -# (q# *# b#)) v# (u# -# (q# *# v#))
        | otherwise = GF# ((x# *# (u# +# m#)) `remInt#` m#)
  fromRational _ = undefined

instance U.IsoUnbox (GF p) Int32 where
  toURepr = coerce (fromIntegral @Int @Int32)
  {-# INLINE toURepr #-}
  fromURepr = coerce (fromIntegral @Int32 @Int)
  {-# INLINE fromURepr #-}

newtype instance UM.MVector s (GF p) = MV_GF (UM.MVector s Int32)
newtype instance U.Vector (GF p) = V_GF (U.Vector Int32)
deriving via (GF p `U.As` Int32) instance GM.MVector U.MVector (GF p)
deriving via (GF p `U.As` Int32) instance G.Vector U.Vector (GF p)
instance U.Unbox (GF p)
