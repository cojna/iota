{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Math.CombinatricsSpec (main, spec) where

import qualified Data.Vector.Unboxed as U

import Data.GaloisField
import Math.Combinatrics hiding (comb, fact, perm)
import qualified Math.Combinatrics
import Test.Prelude

fact :: Int -> GF 1000000007
fact = Math.Combinatrics.fact

perm :: Int -> Int -> GF 1000000007
perm = Math.Combinatrics.perm

comb :: Int -> Int -> GF 1000000007
comb = Math.Combinatrics.comb

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "fact" $ do
    it "fact 0 = 1" $ do
      fact 0 `shouldBe` 1
    it "fact 1 = 1" $ do
      fact 1 `shouldBe` 1
    it "fact 2 = 2" $ do
      fact 2 `shouldBe` 2
    it "fact 10 = 3628800" $ do
      fact 10 `shouldBe` 3628800
    it "fact 100 = 437918130 (mod 1000000007)" $ do
      fact 100 `shouldBe` 437918130
  describe "perm" $ do
    it "perm 10 0 == 1" $ do
      perm 10 0 `shouldBe` 1
    it "prem 10 1 == 10" $ do
      perm 10 1 `shouldBe` 10
    it "perm 10 2 == 90" $ do
      perm 10 2 `shouldBe` 90
    it "perm 10 9 == 3628800" $ do
      perm 10 9 `shouldBe` 3628800
    it "perm 10 10 == 3628800" $ do
      perm 10 10 `shouldBe` 3628800
    prop "perm n n == fact n" prop_permNN
  describe "comb" $ do
    prop "comb n 0 = 1" prop_combN0
    prop "comb n n = 1" prop_combNN
    prop
      "comb n k = comb (n - 1) (k - 1) + comb (n - 1) k"
      prop_constructPascal'sTriangle
    prop "comb n k = comb n (n - k)" prop_combSym
    prop "comb n k = perm n k / fact k" prop_combByPerm
  describe "fact/recipFact cache" $ do
    it "fact * recipFact = 1" $
      U.zipWith (*) factCache (recipFactCache @1000000007)
        `shouldSatisfy` U.all (== 1)
  describe "combNaive" $ do
    specify "combNaive 0 0 == 1" $ do
      combNaive 0 0 `shouldBe` 1
    specify "combNaive maxBound 1 == maxBound" $ do
      combNaive maxBound 1 `shouldBe` maxBound
    specify "combNaive 123456789 2 == 7620789313366866" $ do
      combNaive 123456789 2 `shouldBe` 7620789313366866
    specify "combNaive 64 32 == 1832624140942590534" $ do
      combNaive 64 32 `shouldBe` 1832624140942590534
    specify "combNaive 123 456 == 0" $ do
      combNaive 123 456 `shouldBe` 0
    specify "combNaive 0 (-1) == 0" $ do
      combNaive 0 (-1) `shouldBe` 0
  describe "combSmall" $ do
    prop "combSmall 5 3 (mod 3) == 1" $ do
      combSmall @3 5 3 `shouldBe` 1
  describe "combSmallTable" $ do
    specify "mod 3" $ do
      combSmallTable @3
        `shouldBe` U.fromListN 9 [1, 0, 0, 1, 1, 0, 1, 2, 1]

normalize :: Int -> Int
normalize = flip mod defaultFactCacheSize

prop_permNN :: NonNegative Int -> Bool
prop_permNN (normalize . getNonNegative -> n) =
  perm n n == fact n

prop_combN0 :: NonNegative Int -> Bool
prop_combN0 (normalize . getNonNegative -> n) =
  comb n 0 == 1

prop_combNN :: NonNegative Int -> Bool
prop_combNN (normalize . getNonNegative -> n) =
  comb n n == 1

prop_constructPascal'sTriangle ::
  Positive Int -> Positive Int -> Bool
prop_constructPascal'sTriangle
  (normalize . getPositive -> x)
  (normalize . getPositive -> y) =
    not (n - 1 >= k) || comb n k == comb (n - 1) (k - 1) + comb (n - 1) k
    where
      n = max 1 $ max x y
      k = max 1 $ min x y

prop_combSym ::
  Positive Int -> Positive Int -> Bool
prop_combSym
  (normalize . getPositive -> x)
  (normalize . getPositive -> y) =
    comb n k == comb n (n - k)
    where
      n = max x y
      k = min x y

prop_combByPerm ::
  Positive Int -> Positive Int -> Bool
prop_combByPerm
  (normalize . getPositive -> x)
  (normalize . getPositive -> y) =
    comb n k == perm n k / fact k
    where
      n = max x y
      k = min x y
