{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Data.IntMod.GF where

import           Data.Coerce
import           Data.Proxy
import           GHC.Exts
import           GHC.TypeLits

type IntMod = GF (1000000007 :: Nat)

intMod :: Int -> IntMod
intMod x = GF $ x `mod` m
  where
    m = modulus (proxy# :: Proxy# IntMod)

newtype GF (p :: Nat) = GF{ unGF :: Int } deriving Eq

class Modulus m where
    modulus :: Proxy# m -> Int

instance (KnownNat p) => Modulus (GF p) where
    modulus _ = fromIntegral $ natVal' (proxy# :: Proxy# p)
    {-# INLINE modulus #-}

instance Show (GF p) where
    show = show . unGF

instance (KnownNat p) => Num (GF p) where
    x + y = case coerce x + coerce y of
        xy | xy < m -> coerce xy
           | otherwise -> coerce (xy - m)
      where
        m = modulus (proxy# :: Proxy# (GF p))
    x - y = case coerce x - coerce y of
        xy | xy < 0 -> coerce $ xy + m
           | otherwise -> coerce xy
      where
        m = modulus (proxy# :: Proxy# (GF p))
    x * y = GF $ coerce x * coerce y `rem` modulus (proxy# :: Proxy# (GF p))
    abs = id
    signum = id
    fromInteger x = GF . fromIntegral $ x `mod` fromIntegral m
      where
        m = modulus (proxy# :: Proxy# (GF p))

instance (KnownNat p) => Fractional (GF p) where
    recip x = coerce $ go (coerce x) m 1 0
      where
        m = modulus (proxy# :: Proxy# (GF p))
        go !a !b !u !v
            | b > 0 = case a `quot` b of
                q -> go b (a - (q * b)) v (u - (q * v))
            | otherwise = u `mod` m
    fromRational _ = undefined
