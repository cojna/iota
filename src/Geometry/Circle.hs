{-# LANGUAGE BangPatterns #-}

module Geometry.Dim2.Circle where

import           Geometry

data Circle a = Circle
    { centerC :: !(Point a)  -- ^ center
    , radiusC :: !a          -- ^ radius
    } deriving (Eq, Show)

inCircle :: (Num a, Ord a) => Point a -> Circle a -> Bool
inCircle (P x0 y0) (Circle (P x y) r) = dx * dx + dy * dy <= r * r
  where
    !dx = x - x0
    !dy = y - y0

triangleCenter
    :: (Fractional a)
    => a -> a -> a
    -> Point a -> Point a -> Point a
    -> Point a
triangleCenter wa wb wc a b c
    = fmap (recip (wa + wb + wc) *)
        (fmap (wa *) a + fmap (wb *) b + fmap (wc *) c)

incenter :: (Floating a)
    => Point a -> Point a -> Point a -> Point a
incenter a b c
    = triangleCenter aa bb cc a b c
  where
    cc = norm (a - b)
    aa = norm (b - c)
    bb = norm (c - a)

centroid :: (Fractional a) => Point a -> Point a -> Point a -> Point a
centroid a b c = fmap (recip 3.0 *) (a + b + c)

circumcenter :: (Fractional a) => Point a -> Point a -> Point a -> Point a
circumcenter a b c
    = triangleCenter
        (aa * (bb + cc - aa)) (bb * (cc + aa - bb)) (cc * (aa + bb - cc))
        a b c
  where
    aa = sqrNorm (b - c)
    bb = sqrNorm (c - a)
    cc = sqrNorm (a - b)

orthocenter :: (Num a) => Point a -> Point a -> Point a -> Point a
orthocenter a b c = a + b + c
