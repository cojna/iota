module Data.Graph.MinCostFlowSpec (main, spec) where

import           Data.Graph.MinCostFlow
import           Test.Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "runMinCostFlow" $ do
        it "can flow" $ do
            let n = 5
            mcfb <- newMinCostFlowBuilder n
            addEdgeMCFB 0 1 2 10 mcfb
            addEdgeMCFB 0 2 4 2 mcfb
            addEdgeMCFB 1 2 6 6 mcfb
            addEdgeMCFB 1 3 2 6 mcfb
            addEdgeMCFB 3 2 3 3 mcfb
            addEdgeMCFB 2 4 2 5 mcfb
            addEdgeMCFB 3 4 6 8 mcfb
            mcf <- buildMinCostFlow mcfb
            let source = 0
            let sink = 4
            runMinCostFlow source sink 9 mcf `shouldReturn` Just 80
        it "cannot flow" $ do
            let n = 5
            mcfb <- newMinCostFlowBuilder n
            addEdgeMCFB 0 1 2 10 mcfb
            addEdgeMCFB 0 2 4 2 mcfb
            addEdgeMCFB 1 2 6 6 mcfb
            addEdgeMCFB 1 3 2 6 mcfb
            addEdgeMCFB 3 2 3 3 mcfb
            addEdgeMCFB 2 4 2 5 mcfb
            addEdgeMCFB 3 4 6 8 mcfb
            mcf <- buildMinCostFlow mcfb
            let source = 0
            let sink = 4
            runMinCostFlow source sink 100 mcf `shouldReturn` Nothing
