{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.Semiring where

import Data.Bits
import Data.Primitive

newtype MaxPlus a = MaxPlus {getMaxPlus :: a}
  deriving newtype (Eq, Ord, Show, Prim)

instance (Ord a, Bounded a, Num a) => Num (MaxPlus a) where
  {-# SPECIALIZE instance Num (MaxPlus Int) #-}
  (+) = max
  {-# INLINE (+) #-}
  (-) = undefined
  {-# INLINE (-) #-}
  (*) = (+)
  {-# INLINE (*) #-}
  negate = undefined
  {-# INLINE negate #-}
  abs = id
  {-# INLINE abs #-}
  signum = id
  {-# INLINE signum #-}
  fromInteger 0 = MaxPlus minBound
  fromInteger _ = MaxPlus 0
  {-# INLINE fromInteger #-}

newtype MinPlus a = MinPlus {getMinPlus :: a}
  deriving newtype (Eq, Ord, Show, Prim)

instance (Ord a, Bounded a, Num a) => Num (MinPlus a) where
  {-# SPECIALIZE instance Num (MinPlus Int) #-}
  (+) = min
  {-# INLINE (+) #-}
  (-) = undefined
  {-# INLINE (-) #-}
  (*) = (+)
  {-# INLINE (*) #-}
  negate = undefined
  {-# INLINE negate #-}
  abs = id
  {-# INLINE abs #-}
  signum = id
  {-# INLINE signum #-}
  fromInteger 0 = MinPlus maxBound
  fromInteger _ = MinPlus 0
  {-# INLINE fromInteger #-}

newtype XorAnd a = XorAnd {getXorAnd :: a}
  deriving newtype (Eq, Show, Bits, Prim)

instance (Bits a) => Num (XorAnd a) where
  {-# SPECIALIZE instance Num (XorAnd Int) #-}
  (+) = xor
  {-# INLINE (+) #-}
  (-) = xor
  {-# INLINE (-) #-}
  (*) = (.&.)
  {-# INLINE (*) #-}
  negate = id
  {-# INLINE negate #-}
  abs = id
  {-# INLINE abs #-}
  signum = id
  {-# INLINE signum #-}
  fromInteger x
    | x .&. 1 == 0 = zeroBits
    | otherwise = complement zeroBits
  {-# INLINE fromInteger #-}
