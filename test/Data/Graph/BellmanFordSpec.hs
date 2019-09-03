{-# LANGUAGE OverloadedLists #-}

module Data.Graph.BellmanFordSpec (main, spec) where

import           Data.Graph.BellmanFord
import qualified Data.Vector.Unboxed    as U
import           Test.Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "bellmanFord" $ do
        it "non connected" $ do
            bellmanFord 2 0 [] `shouldBe` [0, maxBound]
        it "negative self loop" $ do
            bellmanFord 1 0 [(0, 0, -1)] `shouldBe` [minBound]
        it "small negative cycle" $ do
            bellmanFord 3 0 [(0, 1, 0), (1, 1, -1), (1, 2, 0), (0, 2, -100000000)]
                `shouldBe` [0, minBound, minBound]
        it "large negative cycle" $ do
            bellmanFord 10 0 (U.generate 10 $ \i -> (i, rem (i + 1) 10, -1))
                `shouldBe` U.replicate 10 minBound
        it "unreachable negative cycle" $ do
            bellmanFord 3 0 [(0, 1, 1), (2, 2, -1), (2, 1, 1)]
                `shouldBe` [0, 1, maxBound]
