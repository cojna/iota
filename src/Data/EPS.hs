{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Data.EPS where

import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U

eps :: (Fractional a) => a
eps = 1e-8
{-# INLINE eps #-}

absErr :: Double -> Double -> Double
absErr ans x = abs (x - ans)

relErr :: Double -> Double -> Double
relErr ans x = abs $ (x - ans) / ans

newtype EPS a = EPS {getEPS :: a}
  deriving newtype (Show, Read, Num, Fractional, Floating)

instance (Num a, Ord a, Fractional a) => Eq (EPS a) where
  {-# SPECIALIZE instance Eq (EPS Double) #-}
  (EPS x) == (EPS y) = abs (y - x) < eps

instance (Num a, Ord a, Fractional a) => Ord (EPS a) where
  {-# SPECIALIZE instance Ord (EPS Double) #-}
  compare (EPS x) (EPS y)
    | abs (x - y) < eps = EQ
    | otherwise = compare x y
  (EPS x) < (EPS y) = x < y - eps
  (EPS x) <= (EPS y) = x < y + eps
  (EPS x) > (EPS y) = x > y + eps
  (EPS x) >= (EPS y) = x > y - eps

newtype instance U.MVector s (EPS a) = MV_EPS (U.MVector s a)
newtype instance U.Vector (EPS a) = V_EPS (U.Vector a)
deriving newtype instance (U.Unbox a) => GM.MVector U.MVector (EPS a)
deriving newtype instance (U.Unbox a) => G.Vector U.Vector (EPS a)
instance (U.Unbox a) => U.Unbox (EPS a)
