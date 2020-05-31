{-# LANGUAGE BangPatterns, DataKinds, DerivingStrategies            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses, RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications, TypeFamilies                         #-}

module Data.GaloisField where

import           Control.Monad
import           Data.Coerce
import           Data.Proxy
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM
import           GHC.Exts
import           GHC.TypeLits

newtype GF (p :: Nat) = GF{ unGF :: Int }
    deriving (Eq)
    deriving newtype (Show)

mkGF :: forall p . KnownNat p => Int -> GF p
mkGF x = GF (x `mod` modulusVal (Proxy @p))

modulusVal :: KnownNat n => proxy n -> Int
modulusVal = fromIntegral . natVal
{-# INLINE modulusVal #-}

-- |
-- >>> withModulus 998244353 $ \proxy -> print (modulusVal proxy)
-- 998244353
withModulus :: (Integral i) => i -> (forall n.KnownNat n => Proxy n -> a) -> a
withModulus n f = case someNatVal (fromIntegral n) of
    Just (SomeNat proxy) -> f proxy
    Nothing              -> error "withModulus failed"

instance (KnownNat p) => Num (GF p) where
    x + y = case coerce x + coerce y of
        xy | xy < m -> coerce xy
           | otherwise -> coerce (xy - m)
      where
        m = modulusVal (Proxy @p)
    x - y = case coerce x - coerce y of
        xy | xy < 0 -> coerce $ xy + modulusVal (Proxy @p)
           | otherwise -> coerce xy
    x * y = GF $ coerce x * coerce y `rem` modulusVal (Proxy @p)
    abs = id
    signum = const (GF 1)
    fromInteger x = GF . fromIntegral $ x `mod` fromIntegral m
      where
        m = modulusVal (Proxy @p)

instance (KnownNat p) => Fractional (GF p) where
    recip x = coerce $ go (coerce x) m 1 0
      where
        !m = modulusVal (Proxy @p)
        go !a !b !u !v
            | b > 0 = case a `quot` b of
                q -> go b (a - (q * b)) v (u - (q * v))
            | otherwise = u `mod` m
    fromRational _ = undefined

newtype instance UM.MVector s (GF p) = MV_GF (UM.MVector s Int)
newtype instance U.Vector (GF p)= V_GF (U.Vector Int)

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
