{-# LANGUAGE BangPatterns #-}

module Geometry.Dim2.Circle where

import           Geometry.Dim2.Base

data Circle = Circle
    { centerC :: !Point  -- ^ center
    , radiusC :: !Double -- ^ radius
    } deriving (Show)

instance Eq Circle where
    (Circle p0 r0) == (Circle p1 r1) = p0 == p1 && eqEPS r0 r1

inCircle :: Point -> Circle -> Bool
inCircle (P x0 y0) (Circle (P x y) r) = dx * dx + dy * dy <= r' * r'
  where
    !dx = x - x0
    !dy = y - y0
    !r' = r + eps

triangleCenter
    :: Double -> Double -> Double
    -> Point -> Point -> Point
    -> Point
triangleCenter wa wb wc a b c
    = recip (wa + wb + wc)
    *: (wa *: a + wb *: b + wc *: c)

incenter :: Point -> Point -> Point -> Point
incenter a b c
    = triangleCenter aa bb cc a b c
  where
    cc = norm2 (a - b)
    aa = norm2 (b - c)
    bb = norm2 (c - a)

centroid :: Point -> Point -> Point -> Point
centroid a b c = recip 3.0 *: (a + b + c)

circumcenter :: Point -> Point -> Point -> Point
circumcenter a b c
    = triangleCenter
        (aa * (bb + cc - aa)) (bb * (cc + aa - bb)) (cc * (aa + bb - cc))
        a b c
  where
    aa = sqrNorm2 (b - c)
    bb = sqrNorm2 (c - a)
    cc = sqrNorm2 (a - b)

orthocenter :: Point -> Point -> Point -> Point
orthocenter a b c = a + b + c
