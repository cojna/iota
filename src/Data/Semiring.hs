{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Semiring where

import Data.Bits
import Data.Primitive
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U

newtype MaxPlus a = MaxPlus {getMaxPlus :: a}
  deriving newtype (Eq, Ord, Show, Prim)

{- |
>>> MaxPlus (1 :: Int) - MaxPlus 0
*** Exception: Prelude.undefined
>>> negate (MaxPlus (1 :: Int))
*** Exception: Prelude.undefined
-}
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

newtype instance U.MVector s (MaxPlus a) = MV_MaxPlus (U.MVector s a)
newtype instance U.Vector (MaxPlus a) = V_MaxPlus (U.Vector a)
deriving newtype instance (U.Unbox a) => GM.MVector U.MVector (MaxPlus a)
deriving newtype instance (U.Unbox a) => G.Vector U.Vector (MaxPlus a)
instance (U.Unbox a) => U.Unbox (MaxPlus a)

newtype MinPlus a = MinPlus {getMinPlus :: a}
  deriving newtype (Eq, Ord, Show, Prim)

{- |
>>> MinPlus (1 :: Int) - MinPlus 0
*** Exception: Prelude.undefined
>>> negate (MinPlus (1 :: Int))
*** Exception: Prelude.undefined
-}
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

newtype instance U.MVector s (MinPlus a) = MV_MinPlus (U.MVector s a)
newtype instance U.Vector (MinPlus a) = V_MinPlus (U.Vector a)
deriving newtype instance (U.Unbox a) => GM.MVector U.MVector (MinPlus a)
deriving newtype instance (U.Unbox a) => G.Vector U.Vector (MinPlus a)
instance (U.Unbox a) => U.Unbox (MinPlus a)

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

newtype instance U.MVector s (XorAnd a) = MV_XorAnd (U.MVector s a)
newtype instance U.Vector (XorAnd a) = V_XorAnd (U.Vector a)
deriving newtype instance (U.Unbox a) => GM.MVector U.MVector (XorAnd a)
deriving newtype instance (U.Unbox a) => G.Vector U.Vector (XorAnd a)
instance (U.Unbox a) => U.Unbox (XorAnd a)
