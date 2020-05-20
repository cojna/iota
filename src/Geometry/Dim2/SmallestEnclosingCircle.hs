{-# LANGUAGE BangPatterns, FlexibleContexts #-}

module Geometry.Dim2.SmallestEnclosingCircle where

import           Data.Function
import qualified Data.List               as L
import qualified Data.Vector.Generic     as G
import           Geometry.Dim2.Base
import           Geometry.Dim2.Circle
import           System.Random.XoRoShiRo

smallestEnclosingCircle :: (G.Vector v Point) => v Point -> Circle
smallestEnclosingCircle = welzl [] . G.toList -- . shuffle
  where
    welzl rs [] = naiveSmallestEnclosingCircle rs
    welzl rs@[_,_,_] _ = naiveSmallestEnclosingCircle rs
    welzl rs (p:ps)
        | inCircle p c = c
        | otherwise = welzl (p:rs) ps
      where
        c = welzl rs ps

naiveSmallestEnclosingCircle :: [Point] -> Circle
naiveSmallestEnclosingCircle [] = Circle (P 0.0 0.0) 0.0
naiveSmallestEnclosingCircle [p] = Circle p 0.0
naiveSmallestEnclosingCircle [p0, p1]
    = Circle (0.5 *: (p0 + p1)) (norm2 (p1 - p0) * 0.5)
naiveSmallestEnclosingCircle ps = snd $ L.minimumBy (compare `on` fst)
    [(r, Circle c r)|c<-centers, let !r=radius c]
  where
    centers = [0.5 *: (p0 + p1)|p0<-ps, p1<-ps]
        ++ [circumcenter p0 p1 p2
           |p0<-ps, p1<-ps, p2<-ps, abs (area p0 p1 p2) > eps
           ]
    radius c = maximum[norm2 (p - c)|p<-ps]
