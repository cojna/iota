{-# LANGUAGE ViewPatterns #-}

module Geometry.Dim2.ConvexHull where

import qualified Data.List          as L

import           Data.VecStack
import           Geometry.Dim2.Base

convexHull :: [Point] -> [Point]
convexHull [] = []
convexHull (L.sort -> (leftest:sorted))
    = reverse . go [leftest] $ L.sortBy (compareCCW leftest) sorted
  where
     go (p:q:conv) (r:rs) = case compareCCW q p r of
        LT -> go (r:p:q:conv) rs
        GT -> go (q:conv) (r:rs)
        EQ | (q - p) `dot` (r - p) < 0 -> go (r:q:conv) rs -- q--p--r
           | otherwise                 -> go (p:q:conv) rs -- q--r--p
     go conv (r:rs) = go (r:conv) rs
     go conv [] = conv
