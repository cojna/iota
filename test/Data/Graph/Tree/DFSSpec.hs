module Data.Graph.Tree.DFSSpec (main, spec) where

import           Data.Graph.Tree.DFS
import qualified Data.Vector.Unboxed as U
import           Test.Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "shortestPath" $ do
        it "sequence tree" $ do
            let tree = undirectedWGraph (U.replicate 10 ())
                     $ U.fromList [(i, i + 1, 1)|i<-[0..8]]
            shortestPath tree 0 `shouldBe` U.generate 10 id
        it "binary tree" $ do
            let tree = undirectedWGraph (U.replicate 7 ())
                     $ U.fromList
                        [(1, 0, 1), (2, 0, 2)
                        ,(3, 1, 1), (4, 1, 1)
                        ,(5, 2, 1), (6, 2, 1)
                        ]
            let ans = U.fromList [0, 1, 2, 2, 2, 3, 3]
            shortestPath tree 0 `shouldBe` ans
        it "star tree" $ do
            let tree = undirectedWGraph (U.replicate 10 ())
                     $ U.fromList [(i, 0, i)|i<-[1..9]]
            shortestPath tree 0 `shouldBe` U.generate 10 id
        it "root only" $ do
            let tree = undirectedWGraph (U.singleton ()) U.empty
            shortestPath tree 0 `shouldBe` U.singleton 0
    describe "diameter" $ do
        it "sequence" $ do
            let tree = undirectedWGraph (U.replicate 10 ())
                     $ U.fromList [(i, i + 1, 1)|i<-[0..8]]
            diameter tree `shouldBe` sum[1|i<-[0..8]]
        it "binary tree" $ do
            let tree = undirectedWGraph (U.replicate 7 ())
                     $ U.fromList
                        [(1, 0, 1), (2, 0, 2)
                        ,(3, 1, 1), (4, 1, 1)
                        ,(5, 2, 1), (6, 2, 1)
                        ]
            diameter tree `shouldBe` (1 + 1 + 2 + 1)
        it "star tree" $ do
            let tree = undirectedWGraph (U.replicate 10 ())
                     $ U.fromList [(i, 0, i)|i<-[1..9]]
            diameter tree `shouldBe` (8 + 9)
        it "root only" $ do
            let tree = undirectedWGraph (U.singleton ()) U.empty
            diameter tree `shouldBe` 0
