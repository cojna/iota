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

sumTo ::
  (Num a, U.Unbox a, PrimMonad m) =>
  FenwickTree (PrimState m) (Sum a) ->
  Int ->
  m a
sumTo ft k = getSum <$> mappendTo ft k

sumFromTo ::
  (Num a, U.Unbox a, PrimMonad m) =>
  FenwickTree (PrimState m) (Sum a) ->
  Int ->
  Int ->
  m a
sumFromTo ft l r = (-) <$> sumTo ft r <*> sumTo ft l

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
  let query = [(i, j) | i <- [0 .. n - 1], j <- [i + 1 .. n]]
  res <- run $ do
    ft <- buildFenwickTree vec
    forM query $ \(i, j) -> do
      sumFromTo ft i j
  let ans = [j - i | (i, j) <- query]
  assert $ map getSum res == ans
