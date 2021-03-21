{-# LANGUAGE ViewPatterns #-}

module Data.FenwickTreeSpec (main, spec) where

import Control.Monad
import Control.Monad.Primitive
import Data.FenwickTree
import Data.Monoid
import qualified Data.Vector.Unboxed as U
import Test.Prelude

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
      let vec = U.replicate 5 (0 :: Sum Int)
      ft <- buildFenwickTree vec
      res <- findMaxIndexLT ft 0
      res `shouldBe` 0
    it "findMaxIndexLT 1 [0,0,0,0,0] == 5" $ do
      let vec = U.replicate 5 (0 :: Sum Int)
      ft <- buildFenwickTree vec
      res <- findMaxIndexLT ft 1
      res `shouldBe` U.length vec
    it "findMaxIndexLT 6 [1,2,3,4,5] == 2" $ do
      let vec = U.generate 5 (Sum . (+ 1))
      ft <- buildFenwickTree vec
      res <- findMaxIndexLT ft 6
      res `shouldBe` 2
    it "findMaxIndexLT 7 [1,2,3,4,5] == 3" $ do
      let vec = U.generate 5 (Sum . (+ 1))
      ft <- buildFenwickTree vec
      res <- findMaxIndexLT ft 7
      res `shouldBe` 3

naiveBuildFenwickTree ::
  (PrimMonad m) =>
  U.Vector (Sum Int) ->
  m (FenwickTree (PrimState m) (Sum Int))
naiveBuildFenwickTree vec = do
  ft <- newFenwickTree (U.length vec)
  flip U.imapM_ vec $ \i x -> do
    mappendAt ft i x
  return ft

prop_naiveBuildFenwickTree :: [Sum Int] -> Property
prop_naiveBuildFenwickTree (U.fromList -> vec) = monadicIO $ do
  ft <- run $ U.freeze . getFenwickTree =<< buildFenwickTree vec
  ft' <- run $ U.freeze . getFenwickTree =<< naiveBuildFenwickTree vec
  assert $ ft == ft'

prefixSums :: U.Vector (Sum Int) -> U.Vector (Sum Int)
prefixSums = U.postscanl' mappend mempty

prop_naiveSumTo :: [Sum Int] -> Property
prop_naiveSumTo (U.fromList -> vec) = monadicIO $ do
  let n = U.length vec
  res <- run $ do
    ft <- buildFenwickTree vec
    U.generateM n $ \i -> do
      Sum <$> sumTo ft (i + 1)
  assert $ res == prefixSums vec

prop_naiveSumFromTo :: NonNegative Int -> Property
prop_naiveSumFromTo (getNonNegative -> n) = monadicIO $ do
  let vec = U.replicate n 1
  let query = [(i, j) | i <- [0 .. n -1], j <- [i + 1 .. n]]
  res <- run $ do
    ft <- buildFenwickTree vec
    forM query $ \(i, j) -> do
      sumFromTo ft i j
  let ans = [j - i | (i, j) <- query]
  assert $ map getSum res == ans

prop_findMaxIndexLTforOnes :: NonNegative Int -> Property
prop_findMaxIndexLTforOnes (getNonNegative -> n) = monadicIO $ do
  let vec = U.replicate n (1 :: Sum Int)
  res <- run $ do
    ft <- buildFenwickTree vec
    forM [1 .. n] $ \i -> do
      findMaxIndexLT ft (Sum i)
  let ans = [0 .. n -1]
  assert $ res == ans
