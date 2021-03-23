{-# LANGUAGE OverloadedLists #-}

module Data.Graph.Tree.HLDSpec (main, spec) where

import Data.Graph.Sparse
import Data.Graph.Tree.HLD
import qualified Data.List as L
import Test.Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "buildUndirectedGraph 5 [(0,1),(0,2),(1,3),(1,4)]" $ do
    let n = 5
    let gr = buildUndirectedGraph n [(0, 1), (0, 2), (1, 3), (1, 4)]
    describe "buildHLD 0 gr" $ do
      let hld = buildHLD 0 gr
      it "lcaHLD 1 2 == 0" $ do
        lcaHLD hld 1 2 `shouldBe` 0
      it "lcaHLD 3 4 == 1" $ do
        lcaHLD hld 3 4 `shouldBe` 1
      it "lcaHLD 0 1 == 0" $ do
        lcaHLD hld 0 1 `shouldBe` 0
      it "lcaHLD 2 3 == 0" $ do
        lcaHLD hld 2 3 `shouldBe` 0

    describe "buildHLD 1 gr" $ do
      let hld = buildHLD 1 gr
      it "lcaHLD 1 2 == 1" $ do
        lcaHLD hld 1 2 `shouldBe` 1
      it "lcaHLD 3 4 == 1" $ do
        lcaHLD hld 3 4 `shouldBe` 1
      it "lcaHLD 0 1 == 1" $ do
        lcaHLD hld 0 1 `shouldBe` 1
      it "lcaHLD 2 3 == 1" $ do
        lcaHLD hld 2 3 `shouldBe` 1

  describe "buildUndirectedGraph 5 [(0,1),(1,2),(2,3),(1,4)]" $ do
    let n = 5
    let gr = buildUndirectedGraph n [(0, 1), (1, 2), (2, 3), (1, 4)]
    describe "buildHLD 0 gr" $ do
      let hld = buildHLD 0 gr
      it "pathHLD hld 0 1 = [(1,2)]" $ do
        pathHLD hld 0 1 `shouldBe` [(1, 2)]
      it "pathHLD hld 0 3 = [(1,4)]" $ do
        pathHLD hld 0 3 `shouldBe` [(1, 4)]
      it "pathHLD hld 3 4 = [(1,4),(4,5)]" $ do
        L.sort (pathHLD hld 3 4)
          `shouldBe` [(2, 4), (4, 5)]
