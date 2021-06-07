{-# LANGUAGE OverloadedLists #-}

module Data.Graph.Sparse.BFSSpec (main, spec) where

import Data.Graph.Sparse
import Data.Graph.Sparse.BFS
import Test.Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "bfsCSR" $ do
    it "empty edge" $ do
      bfsCSR 0 (buildDirectedGraph 1 [])
        `shouldBe` [0]
    it "single edge" $ do
      bfsCSR 0 (buildDirectedGraph 2 [(0, 1)])
        `shouldBe` [0, 1]
    it "star" $ do
      bfsCSR 0 (buildDirectedGraph 4 [(0, 1), (0, 2), (0, 3)])
        `shouldBe` [0, 1, 1, 1]
    it "loop" $ do
      bfsCSR 0 (buildUndirectedGraph 4 [(0, 1), (1, 2), (2, 3), (3, 0)])
        `shouldBe` [0, 1, 2, 1]
    it "self loop" $ do
      bfsCSR 0 (buildDirectedGraph 2 [(0, 1), (1, 1)])
        `shouldBe` [0, 1]
