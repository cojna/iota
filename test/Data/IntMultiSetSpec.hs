{-# LANGUAGE OverloadedLists #-}

module Data.IntMultiSetSpec (main, spec) where

import Data.IntMultiSet
import Test.Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "emptyIMS" $ do
        it "emptyIMS == []" $ do
            emptyIMS `shouldBe` []
    describe "singletonIMS" $ do
        it "singletonIMS 0 == [0]" $ do
            singletonIMS 0 `shouldBe` [0]
    describe "insertIMS" $ do
        it "insertIMS 0 [] == [0]" $ do
            insertIMS 0 [] `shouldBe` [0]
        it "insertIMS 1 [0] == [0, 1]" $ do
            insertIMS 1 [0] `shouldBe` [0, 1]
        it "insertIMS 0 [0] == [0, 0]" $ do
            insertIMS 0 [0] `shouldBe` [0, 0]
    describe "deleteIMS" $ do
        it "deleteIMS 0 [0] == []" $ do
            deleteIMS 0 [0] `shouldBe` []
        it "deleteIMS 0 [0, 0] == [0]" $ do
            deleteIMS 0 [0, 0] `shouldBe` [0]
        it "deleteIMS 1 [0] == [0]" $ do
            deleteIMS 1 [0] `shouldBe` [0]
    describe "findMinIMS" $ do
        it "findMinIMS [0, 0, 1] == 0" $ do
            findMinIMS [0, 0, 1] `shouldBe` 0
        it "findMinIMS [] throw exception" $ do
            evaluate (findMinIMS []) `shouldThrow` anyErrorCall
    describe "findMaxIMS" $ do
        it "findMaxIMS [0, 0, 1] == 1" $ do
            findMaxIMS [0, 0, 1] `shouldBe` 1
        it "findMaxIMS [] throw exception" $ do
            evaluate (findMaxIMS []) `shouldThrow` anyErrorCall
    describe "deleteMinIMS" $ do
        it "deleteMinIMS [0, 0, 1] == [0, 1]" $ do
            deleteMinIMS [0, 0, 1] `shouldBe` [0, 1]
        it "deleteMinIMS [] throw exception" $ do
            evaluate (deleteMinIMS []) `shouldThrow` anyErrorCall
    describe "deleteMaxIMS" $ do
        it "deleteMaxIMS [0, 0, 1] == [0, 0]" $ do
            deleteMaxIMS [0, 0, 1] `shouldBe` [0, 0]
        it "deleteMaxIMS [] throw exception" $ do
            evaluate (deleteMaxIMS []) `shouldThrow` anyErrorCall
    describe "maxViewIMS" $ do
        it "maxViewIMS [0, 0, 1] == Just [0, 0]" $ do
            maxViewIMS [0, 0, 1] `shouldBe` Just (1, [0, 0])
        it "maxViewIMS [] == Nothing" $ do
            maxViewIMS [] `shouldBe` Nothing
    describe "minViewIMS" $ do
        it "minViewIMS [0, 0, 1] == Just [0, 1]" $ do
            minViewIMS [0, 0, 1] `shouldBe` Just (0, [0, 1])
        it "minViewIMS [] == Nothing" $ do
            minViewIMS [] `shouldBe` Nothing
