{-# LANGUAGE BangPatterns, PatternSynonyms #-}

module Geometry.Dim2.Base where

eps :: Double
eps = 1e-8

eqEPS :: Double -> Double -> Bool
eqEPS x y = abs (y - x) < eps

ltEPS :: Double -> Double -> Bool
ltEPS x y = x < y - eps

leEPS :: Double -> Double -> Bool
leEPS x y = x < y + eps

gtEPS :: Double -> Double -> Bool
gtEPS x y = x > y + eps

geEPS :: Double -> Double -> Bool
geEPS x y = x > y - eps

compareEPS :: Double -> Double -> Ordering
compareEPS x y
    | abs (x - y) < eps = EQ
    | otherwise = compare x y

data Point = P !Double !Double

instance Eq Point where
    (P x0 y0) == (P x1 y1) = dx * dx + dy * dy < eps
      where
        !dx = x1 - x0
        !dy = y1 - y0
    {-# INLINE (==) #-}

instance Ord Point where
    compare (P x0 y0) (P x1 y1) = case compareEPS x0 x1 of
        LT -> LT
        EQ -> compareEPS y0 y1
        GT -> GT
    {-# INLINE compare #-}

instance Show Point where
    show (P x y) = show (x, y)

instance Num Point where
    (P x0 y0) + (P x1 y1) = P (x0 + x1) (y0 + y1)
    (P x0 y0) - (P x1 y1) = P (x0 - x1) (y0 - y1)
    (P x0 y0) * (P x1 y1) = P (x0 * x1 - y0 * y1) (x0 * y1 + x1 * y0)
    negate (P x y) = P (negate x) (negate y)
    abs = id
    signum _ = P 1.0 0.0
    fromInteger n = P (fromInteger n) 0

instance Fractional Point where
    v0@(P x0 y0) / v1@(P x1 y1) = P (dot v0 v1 / rr) (cross v1 v0 / rr)
      where
        !rr = sqrNorm2 v1
    fromRational q = P (fromRational q) 0

infixr 7 *:
(*:) :: Double -> Point -> Point
(*:) !k (P x y) = P (k * x) (k * y)
{-# INLINE (*:) #-}

dot :: Point -> Point -> Double
dot (P x0 y0) (P x1 y1) = x0 * x1 + y0 * y1
{-# INLINE dot #-}

cross :: Point -> Point -> Double
cross (P x0 y0) (P x1 y1) = x0 * y1 - y0 * x1
{-# INLINE cross #-}

conjugate :: Point -> Point
conjugate (P x y) = P x (-y)
{-# INLINE conjugate #-}

area :: Point -> Point -> Point -> Double
area o u v = (u - o) `cross` (v - o)
{-# INLINE area #-}

compareCCW :: Point -> Point -> Point -> Ordering
compareCCW o = \u v -> compareEPS 0.0 (area o u v)
{-# INLINE compareCCW #-}

compareCW :: Point -> Point -> Point -> Ordering
compareCW o = flip (compareCCW o)
{-# INLINE compareCW #-}

degToRad :: Double -> Double
degToRad deg = deg / 180.0 * pi
{-# INLINE degToRad #-}

radToDeg :: Double -> Double
radToDeg rad = rad / pi * 180.0
{-# INLINE radToDeg #-}

rotateRad :: Double -> Point -> Point
rotateRad a v = v * P (cos a) (sin a)
{-# INLINE rotateRad #-}

rotateDeg :: Double -> Point -> Point
rotateDeg a v = v * P (cos $ degToRad a) (sin $ degToRad a)
{-# INLINE rotateDeg #-}

norm1 :: Point -> Double
norm1 (P x y) = abs x + abs y
{-# INLINE norm1 #-}

norm2 :: Point -> Double
norm2 = sqrt . sqrNorm2
{-# INLINE norm2 #-}

sqrNorm2 :: Point -> Double
sqrNorm2 v = v `dot` v
{-# INLINE sqrNorm2 #-}

normSup :: Point -> Double
normSup (P x y) = abs x `max` abs y
{-# INLINE normSup #-}

normalize :: Point -> Point
normalize v = recip (norm2 v) *: v
{-# INLINE normalize #-}
