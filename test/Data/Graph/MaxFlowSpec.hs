{-# LANGUAGE TypeApplications #-}

module Data.Graph.MaxFlowSpec (main, spec) where

import Data.Graph.MaxFlow
import Test.Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "maxFlow" $ do
    it "example" $ do
      let n = 5
      maxFlow @Int
        n
        0
        4
        ( \builder -> do
            addEdgeMFB builder (0, 1, 10)
            addEdgeMFB builder (0, 2, 2)
            addEdgeMFB builder (1, 2, 6)
            addEdgeMFB builder (1, 3, 6)
            addEdgeMFB builder (3, 2, 2)
            addEdgeMFB builder (2, 4, 5)
            addEdgeMFB builder (3, 4, 8)
        )
        `shouldBe` 11

    it "contains sink unreachable nodes" $ do
      let n = 3
      withTLEmsec 1000 $ do
        maxFlow @Int
          n
          0
          1
          ( \builder -> do
              addEdgeMFB builder (0, 1, 1)
              addEdgeMFB builder (0, 2, 1)
          )
          `shouldBe` 1

    it "contains loop" $ do
      let n = 4
      withTLEmsec 1000 $ do
        maxFlow @Int
          n
          0
          2
          ( \builder -> do
              addEdgeMFB builder (0, 1, 1000000000)
              addEdgeMFB builder (1, 2, 1000000000)
              addEdgeMFB builder (2, 3, 1000000000)
              addEdgeMFB builder (3, 0, 1000000000)
          )
          `shouldBe` 1000000000

    it "unreachable sink" $ do
      let n = 2
      withTLEmsec 1000 $ do
        maxFlow @Int n 0 1 (const (return ()))
          `shouldBe` 0
