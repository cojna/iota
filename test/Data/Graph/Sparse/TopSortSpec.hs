{-# LANGUAGE OverloadedLists #-}

module Data.Graph.Sparse.TopSortSpec (main, spec) where

import Data.Graph.Sparse
import Data.Graph.Sparse.TopSort
import Test.Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "topSort" $ do
    it "topSort 4 [(2, 1), (1, 0), (3, 2)] = Just [3, 2, 1, 0]" $ do
      topSort (buildDirectedGraph 4 [(2, 1), (1, 0), (3, 2)])
        `shouldBe` Just [3, 2, 1, 0]
    it "topSort 2 [(0, 1), (1, 0)] = Nothing" $ do
      topSort (buildDirectedGraph 2 [(0, 1), (1, 0)])
        `shouldBe` Nothing
    it "topSort 10 [] = Just [0..9]" $ do
      topSort (buildDirectedGraph 10 [])
        `shouldBe` Just [0 .. 9]
