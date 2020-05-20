{-# LANGUAGE BangPatterns #-}

module Algorithm.GoldenSectionSearch where

import           Data.Coerce
import           Data.Ord
import           Data.Semigroup

-- | phi = (1+sqrt 5) / 2
phi :: Double
phi = 1.618033988749895
{-# INLINE phi #-}

-- | resphi = 2-phi = 1/(1+phi)
resphi :: Double
resphi = 0.3819660112501051
{-# INLINE resphi #-}


-- | mid1 (mid1 low high) high = mid2 low high
mid1 :: Double -> Double -> Double
mid1 low high = (low * phi + high) * resphi
{-# INLINE mid1 #-}

-- | mid2 low (mid2 low high) = mid1 low high
mid2 :: Double -> Double -> Double
mid2 low high = (low + high * phi) * resphi
{-# INLINE mid2 #-}

epsGS :: Double
epsGS = 1e-12
{-# INLINE epsGS #-}

goldenSectionSearchMin
    :: (Ord a) => Double -> Double -> (Double -> a) -> ArgMin a Double
goldenSectionSearchMin low high f = go 72 low x1 x2 high (f x1) (f x2)
   where
     !x1 = mid1 low high
     !x2 = mid2 low high
     go !n !x0 !x1 !x2 !x3 !fx1 !fx2
       | n == 0 || abs (x3 - x0) < epsGS = Min (Arg fx1 x1)
       | fx1 < fx2 = let !x = mid1 x0 x2  -- mid2 x0 x2 == x1
                     in go (n - 1) x0 x x1 x2 (f x) fx1
       | otherwise = let !x = mid2 x1 x3  -- mid1 x1 x3 == x2
                     in go (n - 1) x1 x2 x x3 fx2 (f x)

goldenSectionSearchMax
    :: (Ord a) => Double -> Double -> (Double -> a) -> ArgMax a Double
goldenSectionSearchMax low high f = coerce
    $ goldenSectionSearchMin low high (Down . f)

goldenSectionSearchMin2
    :: (Ord a) => Double -> Double -> (Double -> Double -> a)
    -> ArgMin a (Double, Double)
goldenSectionSearchMin2 low high f = Min (Arg fxy (x, y))
  where
    Min (Arg (Min (Arg fxy y)) x) =
        goldenSectionSearchMin low high $ \cx ->
        goldenSectionSearchMin low high $ \cy ->
            f cx cy

goldenSectionSearchMax2
    :: (Ord a) => Double -> Double -> (Double -> Double -> a)
    -> ArgMax a (Double, Double)
goldenSectionSearchMax2 low high f = coerce
    $ goldenSectionSearchMin2 low high (\x y -> Down (f x y))

