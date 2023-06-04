{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Math.PrimeSpec (main, spec) where

import qualified Data.IntMap.Strict as IM
import Math.Prime
import Test.Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "primeFactors" $ do
    prop "factorize" prop_primeFactorsFactorize
    prop "factorize by prime" prop_primeFactctorsFactorizeByPrime
    it "primeFactors 0 == []" $ primeFactors @Int 0 `shouldBe` []
    it "primeFactors 1 == []" $ primeFactors @Int 1 `shouldBe` []
    it "primeFactors 2 == [2]" $ primeFactors @Int 2 `shouldBe` [2]
    it "primeFactors 60 == [2,2,3,5]" $
      primeFactors @Int 60 `shouldBe` [2, 2, 3, 5]
    it "primeFactors 2147483647 == [2147483647]" $
      primeFactors @Int 2147483647 `shouldBe` [2147483647]
    it "primeFactors 999999999989 == [999999999989]" $
      primeFactors @Int 999999999989 `shouldBe` [999999999989]
    it "primeFactors 999999999997 == [5507,181587071]" $
      primeFactors @Int 999999999997 `shouldBe` [5507, 181587071]
  describe "isPrime" $ do
    it "0 is not prime" $ isPrime @Int 0 `shouldBe` False
    it "1 is not prime" $ isPrime @Int 1 `shouldBe` False
    it "2 is prime" $ isPrime @Int 2 `shouldBe` True
    it "2147483647 is prime" $ isPrime @Int 2147483647 `shouldBe` True
    it "999999999989 is prime" $ isPrime @Int 999999999989 `shouldBe` True
    it "999999999997 is not prime" $ isPrime @Int 999999999997 `shouldBe` False
  describe "totient" $ do
    prop "same to naive" prop_totientSameToNaive
    it "totient 0 == 0" $ totient 0 `shouldBe` 0
    it "totient 1 == 1" $ totient 1 `shouldBe` 1
  describe "divisors" $ do
    prop "same to naive" prop_divisorsSameToNaive
    it "divisors 12 == [1,2,3,4,6,12]" $
      divisors 12 `shouldBe` [1, 2, 3, 4, 6, 12]
    it "divisors 1 == [1]" $
      divisors 1 `shouldBe` [1]
    it "length (divisors 735134400) == 1344" $
      length (divisors 735134400) `shouldBe` 1344
  describe "moebius" $ do
    it "moebius 1 == 1" $
      moebius 1 `shouldBe` 1
    it "moebius 2 == -1" $
      moebius 2 `shouldBe` (-1)
    it "moebius 3 == -1" $
      moebius 3 `shouldBe` (-1)
    it "moebius (2 * 2) == 0" $
      moebius (2 * 2) `shouldBe` 0
    it "moebius (2 * 3) == 1" $
      moebius (2 * 3) `shouldBe` 1
    it "moebius (2 * 2 * 3) == 0" $
      moebius (2 * 2 * 3) `shouldBe` 0
  describe "moebiusInversion" $ do
    it "moebiusInvesion 12 (length.divisors)" $
      moebiusInversion 12 (length . divisors)
        `shouldBe` IM.fromList [(1, 1), (2, 1), (3, 1), (4, 1), (6, 1), (12, 1)]
    it "moebiusInvesion 999999999997 (length.divisors)" $
      moebiusInversion 999999999997 (length . divisors)
        `shouldBe` IM.fromList [(1, 1), (5507, 1), (181587071, 1), (999999999997, 1)]

prop_primeFactorsFactorize :: Positive Int -> Bool
prop_primeFactorsFactorize (getPositive -> x) =
  x == product (primeFactors x)

prop_primeFactctorsFactorizeByPrime :: Positive Int -> Bool
prop_primeFactctorsFactorizeByPrime (getPositive -> x) =
  all isPrime $ primeFactors x

naiveTotient :: Int -> Int
naiveTotient x = length [i | i <- [1 .. x], gcd x i == 1]

prop_totientSameToNaive :: Small Word -> Bool
prop_totientSameToNaive (fromIntegral . getSmall -> x) =
  x == 0 || totient x == naiveTotient x

naiveDivisors :: Int -> [Int]
naiveDivisors x = [i | i <- [1 .. x], rem x i == 0]

prop_divisorsSameToNaive :: Small Word -> Bool
prop_divisorsSameToNaive (fromIntegral . getSmall -> x) =
  x == 0 || divisors x == naiveDivisors x
