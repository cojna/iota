module Data.Graph.MinCostFlowSpec (main, spec) where

import Data.Graph.MinCostFlow
import Test.Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "minCostFlow" $ do
    it "example (can flow)" $ do
      minCostFlow
        2
        0
        1
        2
        ( \builder -> do
            addEdgeMCFB builder 0 1 123 2
        )
        `shouldBe` (246 :: Int, 2)
    it "example (cannot flow)" $ do
      minCostFlow
        2
        0
        1
        123456789
        ( \builder -> do
            addEdgeMCFB builder 0 1 123 2
        )
        `shouldBe` (246, 2)
    it "multiple edges" $ do
      minCostFlow
        2
        0
        1
        1
        ( \builder -> do
            addEdgeMCFB builder 0 1 123 1
            addEdgeMCFB builder 0 1 456 1
        )
        `shouldBe` (123, 1)
    it "unreachable sink" $ do
      minCostFlow 2 0 1 1 (const $ return ())
        `shouldBe` (0, 0)
    it "contains loop (can flow)" $ do
      minCostFlow
        4
        0
        2
        10
        ( \builder -> do
            addEdgeMCFB builder 0 1 1 100
            addEdgeMCFB builder 1 2 1 100
            addEdgeMCFB builder 2 3 1 100
            addEdgeMCFB builder 3 0 1 100
        )
        `shouldBe` (20, 10)
    it "contains loop (cannot flow)" $ do
      minCostFlow
        4
        0
        2
        123
        ( \builder -> do
            addEdgeMCFB builder 0 1 1 100
            addEdgeMCFB builder 1 2 1 100
            addEdgeMCFB builder 2 3 1 100
            addEdgeMCFB builder 3 0 1 100
        )
        `shouldBe` (200, 100)
