{-# LANGUAGE OverloadedLists #-}

module Data.DoublingSpec (main, spec) where

import Data.Doubling
import Data.Monoid (Sum (..))
import Test.Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  let next :: Doubling (Sum Int)
      next = generateDoubling 10 (\i -> (mod (i + 1) 10, Sum 1))
  describe "doublingStepN" $ do
    specify "doublingStepN 0 0 mempty" $ do
      doublingStepN 0 0 mempty next
        `shouldBe` (0, mempty)
    specify "doublingStepN 1 0 mempty" $ do
      doublingStepN 1 0 mempty next
        `shouldBe` (1, 1)
    specify "doublingStepN 2 0 mempty" $ do
      doublingStepN 2 0 mempty next
        `shouldBe` (2, 2)
    specify "doublingStepN maxBound 0 mempty" $ do
      doublingStepN maxBound 0 mempty next
        `shouldBe` (rem maxBound 10, Sum maxBound)
  let table :: DoublingTable (Sum Int)
      table = buildDoublingTable next
  describe "doublingStepNQuery" $ do
    specify "doublingStepNQuery 0 0 table" $ do
      doublingStepNQuery 0 0 mempty table
        `shouldBe` (0, mempty)
    specify "doublingStepN 1 0 table" $ do
      doublingStepNQuery 1 0 mempty table
        `shouldBe` (1, 1)
    specify "doublingStepNQuery 2 0 table" $ do
      doublingStepNQuery 2 0 mempty table
        `shouldBe` (2, 2)
    specify "doublingStepNQuery maxBound 0 table" $ do
      doublingStepNQuery maxBound 0 mempty table
        `shouldBe` (rem maxBound 10, Sum maxBound)
