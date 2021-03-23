{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Geometry.SmallestEnclosingCircle where

import Data.Function
import qualified Data.List as L
import qualified Data.Vector.Generic as G
import Geometry
import Geometry.Circle
import System.Random.XoRoShiRo

smallestEnclosingCircle ::
  (Floating a, Ord a, G.Vector v (Point a)) => v (Point a) -> Circle a
smallestEnclosingCircle = welzl [] . G.toList . shuffle
  where
    welzl rs@[_, _, _] _ = trivial rs
    welzl rs [] = trivial rs
    welzl rs (p : ps)
      | inCircle p c = c
      | otherwise = welzl (p : rs) ps
      where
        !c = welzl rs ps

    trivial [] = Circle (P 0.0 0.0) 0.0
    trivial [p] = Circle p 0.0
    trivial [p0, p1] = Circle (fmap (0.5 *) (p0 + p1)) (norm (p1 - p0) * 0.5)
    -- not equal to the smallest enclosing circle
    trivial [p0, p1, p2] = Circle c (norm (p0 - c))
      where
        c = circumcenter p0 p1 p2

naiveSmallestEnclosingCircle :: (Floating a, Ord a) => [Point a] -> Circle a
naiveSmallestEnclosingCircle [] = Circle (P 0.0 0.0) 0.0
naiveSmallestEnclosingCircle [p] = Circle p 0.0
naiveSmallestEnclosingCircle [p0, p1] =
  Circle (fmap (0.5 *) (p0 + p1)) (norm (p1 - p0) * 0.5)
naiveSmallestEnclosingCircle ps =
  snd $
    L.minimumBy
      (compare `on` fst)
      [(r, Circle c r) | c <- centers, let !r = radius c]
  where
    centers =
      [fmap (0.5 *) (p0 + p1) | p0 <- ps, p1 <- ps]
      ++ [ circumcenter p0 p1 p2
         | p0 <- ps
         , p1 <- ps
         , p2 <- ps
         , abs (area p0 p1 p2) > 0
         ]
    radius c = maximum [norm (p - c) | p <- ps]
