{-# LANGUAGE TypeApplications #-}

module Geometry.ConvexHullSpec (main, spec) where

import qualified Data.Vector              as V

import           Data.EPS
import           Geometry
import           Geometry.ConvexHull
import           Geometry.Instances
import qualified System.Random.XoRoShiRo  as Random
import           Test.Prelude             hiding (shuffle)

main :: IO ()
main = hspec spec

shuffleList :: [a] -> [a]
shuffleList = V.toList . Random.shuffle . V.fromList

spec :: Spec
spec = do
    describe "convexHull" $ do
        it "convexHull [] = []" $ do
            convexHull [] `shouldBe` []
        it "convexHull [(0, 0)] = [(0, 0)]" $ do
            convexHull [P 0 0] `shouldBe` [P 0 0]
        it "convexHull [(0, 0), (1, 0)] = [(0, 0), (1, 0)]" $ do
            convexHull [P 0 0, P 1 0] `shouldBe` [P 0 0, P 1 0]
        it "convexHull [(1, 0), (0, 0)] = [(0, 0), (1, 0)]" $ do
            convexHull [P 1 0, P 0 0] `shouldBe` [P 0 0, P 1 0]
        it "convexHull [P (-1) 0, P 0 1, P 1 0]" $ do
            convexHull [P (-1) 0, P 0 1, P 1 0]
                `shouldBe` [P (-1) 0, P 1 0, P 0 1]
        it "convexHull on x-axis" $ do
            let n = 100
                maxX = fromIntegral n - 1
                points = shuffleList [P (fromIntegral x) 0|x<-[0..n-1]]
            convexHull points `shouldBe` [P 0 0, P maxX 0]
        it "convexHull on y-axis" $ do
            let n = 100
                maxY = fromIntegral n - 1
                points = shuffleList [P 0 (fromIntegral y)|y<-[0..n-1]]
            convexHull points `shouldBe` [P 0 0, P 0 maxY]
        it "convexHull dense square" $ do
            let n = 10
                lim = fromIntegral n - 1
                points = shuffleList
                    [ P (fromIntegral x) (fromIntegral y)
                    | x<-[0..n-1], y<-[0..n-1]
                    ]
            convexHull points `shouldBe` [P 0 0, P lim 0, P lim lim, P 0 lim]
        describe "convexHull . convexHull = convexHull" $ do
            prop "Int" (prop_convexHullConvexHull @Int)
            prop "Double" (prop_convexHullConvexHull @(EPS Double))

prop_convexHullConvexHull :: (Num a, Ord a) => [Point a] -> Bool
prop_convexHullConvexHull points
    = convexHull (convexHull points) == convexHull points
