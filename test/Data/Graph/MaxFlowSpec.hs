module Data.Graph.MaxFlowSpec (main, spec) where

import           Data.Graph.MaxFlow
import           Test.Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "runMaxCostFlow" $ do
        it "example" $ do
            let n = 5
            mfb <- newMaxFlowBuilder n
            addEdgeMFB 0 1 10 mfb
            addEdgeMFB 0 2 2 mfb
            addEdgeMFB 1 2 6 mfb
            addEdgeMFB 1 3 6 mfb
            addEdgeMFB 3 2 2 mfb
            addEdgeMFB 2 4 5 mfb
            addEdgeMFB 3 4 8 mfb

            mf <- buildMaxFlow mfb
            runMaxFlow 0 4 mf
                `shouldReturn` (11 :: Int)
        it "contains sink unreachable nodes" $ do
            let n = 3
            mfb <- newMaxFlowBuilder n
            addEdgeMFB 0 1 1 mfb
            addEdgeMFB 0 2 1 mfb

            mf <- buildMaxFlow mfb
            withTLEmsec 10 (runMaxFlow 0 1 mf)
                `shouldReturn` (1 :: Int)

