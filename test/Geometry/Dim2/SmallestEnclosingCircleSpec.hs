{-# LANGUAGE BangPatterns, DataKinds, ViewPatterns #-}

module Geometry.Dim2.SmallestEnclosingCircleSpec (main, spec) where

import           Algorithm.GoldenSectionSearch
import           Data.Semigroup
import qualified Data.Vector                           as V
import           Geometry.Dim2.Base
import           Geometry.Dim2.Circle
import           Geometry.Dim2.Instances
import           Geometry.Dim2.SmallestEnclosingCircle
import           Test.Prelude

import           Debug.Trace

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "smallestEnclosingCircle" $ do
        prop "enclose all points" prop_enclose
        prop "equal to naive" prop_naive
        prop "equal to golden section search" prop_goldenSectionSearch

byGoldenSectionSearch :: V.Vector Point -> Circle
byGoldenSectionSearch points | V.null points = Circle (P 0.0 0.0) 0.0
byGoldenSectionSearch points = Circle (P x y) r
  where
    low = -1000.0
    high = 1000.0
    Min (Arg r (x, y)) = goldenSectionSearchMin2 low high $ \cx cy ->
            let !c = P cx cy
            in V.maximum $ V.map (\p -> norm2 (p - c)) points

prop_naive :: SizeBoundedList 16 Point -> Bool
prop_naive (getSizeBoundedList -> points)
    = smallestEnclosingCircle (V.fromList points) ==
            naiveSmallestEnclosingCircle points

prop_goldenSectionSearch :: [Point] -> Bool
prop_goldenSectionSearch (V.fromList -> points)
    = smallestEnclosingCircle points == byGoldenSectionSearch points

prop_enclose :: [Point] -> Bool
prop_enclose (V.fromList -> points) = V.all (`inCircle` c) points
  where
    !c = smallestEnclosingCircle points
