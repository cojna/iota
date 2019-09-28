{-# LANGUAGE ViewPatterns #-}

module Data.FenwickTreeSpec (main, spec) where

import           Control.Monad
import           Control.Monad.Primitive
import           Data.FenwickTree
import qualified Data.Vector.Unboxed     as U
import           Test.Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "buildFenwickTree" $ do
        prop "naive prop for random lists" prop_naiveBuildFenwickTree
    describe "sumTo" $ do
        prop "naive prop for random lists" prop_naiveSumTo
    describe "sumFromTo" $ do
        prop "naive prop for [1,1..1]" prop_naiveSumFromTo
    describe "findMaxIndexLT" $ do
        prop "findMaxIndexLT k [1,1..1] == k - 1" prop_findMaxIndexLTforOnes
        it "findMaxIndexLT 0 [0,0,0,0,0] == 0" $ do
            let vec = U.replicate 5 (0 :: Int)
            ft <- buildFenwickTree vec
            res <- findMaxIndexLT 0 ft
            res `shouldBe` 0
        it "findMaxIndexLT 1 [0,0,0,0,0] == 5" $ do
            let vec = U.replicate 5 (0 :: Int)
            ft <- buildFenwickTree vec
            res <- findMaxIndexLT 1 ft
            res `shouldBe` U.length vec
        it "findMaxIndexLT 6 [1,2,3,4,5] == 2" $ do
            let vec = U.generate 5 (+1)
            ft <- buildFenwickTree vec
            res <- findMaxIndexLT 6 ft
            res `shouldBe` 2
        it "findMaxIndexLT 7 [1,2,3,4,5] == 3" $ do
            let vec = U.generate 5 (+1)
            ft <- buildFenwickTree vec
            res <- findMaxIndexLT 7 ft
            res `shouldBe` 3


naiveBuildFenwickTree :: (PrimMonad m) => U.Vector Int -> m (FenwickTree (PrimState m) Int)
naiveBuildFenwickTree vec = do
    ft <- newFenwickTree (U.length vec)
    flip U.imapM_ vec $ \i x -> do
        addAt i x ft
    return ft

prop_naiveBuildFenwickTree :: [Int] -> Property
prop_naiveBuildFenwickTree (U.fromList -> vec) = monadicIO $ do
    ft <- run $ getFreezeFenwickTree =<< buildFenwickTree vec
    ft' <- run $ getFreezeFenwickTree =<< naiveBuildFenwickTree vec
    assert $ ft == ft'

prefixSums :: U.Vector Int -> U.Vector Int
prefixSums = U.postscanl' (+) 0

prop_naiveSumTo :: [Int] -> Property
prop_naiveSumTo (U.fromList -> vec) = monadicIO $ do
    let n = U.length vec
    res <- run $ do
        ft <- buildFenwickTree vec
        U.generateM n $ \i -> do
            sumTo (i + 1) ft
    assert $ res == prefixSums vec

prop_naiveSumFromTo :: NonNegative Int -> Property
prop_naiveSumFromTo (getNonNegative -> n) = monadicIO $ do
    let vec = U.replicate n 1
    let query = [(i, j) | i<-[0..n-1],j<-[i+1..n]]
    res <- run $ do
        ft <- buildFenwickTree vec
        forM query $ \(i, j) -> do
            sumFromTo i j ft
    let ans = [j - i | (i, j) <- query]
    assert $ res == ans

prop_findMaxIndexLTforOnes :: NonNegative Int -> Property
prop_findMaxIndexLTforOnes (getNonNegative -> n) = monadicIO $ do
    let vec = U.replicate n (1 :: Int)
    res <- run $ do
        ft <- buildFenwickTree vec
        forM [1..n] $ \i -> do
            findMaxIndexLT i ft
    let ans = [0..n-1]
    assert $ res == ans

