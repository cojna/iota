{-# LANGUAGE OverloadedLists #-}

module Data.Graph.Tree.LCASpec (main, spec) where

import           Data.Graph.Sparse
import           Data.Graph.Tree.LCA
import           Test.Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "buildUndirectedGraph 5 [(0,1),(0,2),(1,3),(1,4)]" $ do
        let n = 5
        let gr = buildUndirectedGraph n [(0,1),(0,2),(1,3),(1,4)]
        describe "buildLCA gr 0" $ do
            let lca = buildLCA gr 0
            it "queryLCA 1 2 == 0" $ do
                queryLCA lca 1 2 `shouldBe` 0
            it "queryLCA 3 4 == 1" $ do
                queryLCA lca 3 4 `shouldBe` 1
            it "queryLCA 0 1 == 0" $ do
                queryLCA lca 0 1 `shouldBe` 0
            it "queryLCA 2 3 == 0" $ do
                queryLCA lca 2 3 `shouldBe` 0
            it "queryDepth lca 0 == 0" $ do
                queryDepth lca 0 `shouldBe` 0
            it "queryDepth lca 1 == 1" $ do
                queryDepth lca 1 `shouldBe` 1
            it "queryDepth lca 2 == 1" $ do
                queryDepth lca 2 `shouldBe` 1
            it "queryDepth lca 3 == 2" $ do
                queryDepth lca 3 `shouldBe` 2
            it "queryDepth lca 4 == 2" $ do
                queryDepth lca 4 `shouldBe` 2
        describe "buildLCA gr 1" $ do
            let lca = buildLCA gr 1
            it "queryLCA 1 2 == 1" $ do
                queryLCA lca 1 2 `shouldBe` 1
            it "queryLCA 3 4 == 1" $ do
                queryLCA lca 3 4 `shouldBe` 1
            it "queryLCA 0 1 == 1" $ do
                queryLCA lca 0 1 `shouldBe` 1
            it "queryLCA 2 3 == 1" $ do
                queryLCA lca 2 3 `shouldBe` 1
            it "queryDepth lca 0 == 1" $ do
                queryDepth lca 0 `shouldBe` 1
            it "queryDepth lca 1 == 0" $ do
                queryDepth lca 1 `shouldBe` 0
            it "queryDepth lca 2 == 2" $ do
                queryDepth lca 2 `shouldBe` 2
            it "queryDepth lca 3 == 1" $ do
                queryDepth lca 3 `shouldBe` 1
            it "queryDepth lca 4 == 1" $ do
                queryDepth lca 4 `shouldBe` 1

