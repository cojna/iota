module Data.EPS where

eps :: Double
eps = 1e-8
{-# INLINE eps #-}

newtype EPS = EPS Double

instance Eq EPS where
    (EPS x) == (EPS y) = abs (x - y) < eps

instance Ord EPS where
    compare (EPS x) (EPS y)
        | abs (x - y) < eps = EQ
        | otherwise = compare x y

instance Show EPS where
    show (EPS x) = show x

absErr :: Double -> Double -> Double
absErr ans x = abs (x - ans)

relErr :: Double -> Double -> Double
relErr ans x = abs $ (x - ans) / ans

eqEPS :: Double -> Double -> Double -> Bool
eqEPS eps ans x = absErr ans x < eps || relErr ans x < eps
