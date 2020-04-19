{-# LANGUAGE ViewPatterns #-}

module Math.PrimeSpec (main, spec) where

import           Data.Int
import qualified Data.List           as L
import qualified Data.Vector.Unboxed as U
import           Data.Word
import           Math.Prime
import           Test.Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "primeFactors" $ do
        prop "factorize" prop_primeFactorsFactorize
        prop "factorize by prime" prop_primeFactctorsFactorizeByPrime
    describe "isPrime" $ do
        it "0 is not prime" $ isPrime 0 `shouldBe` False
        it "1 is not prime" $ isPrime 1 `shouldBe` False
        it "2 is prime" $ isPrime 2 `shouldBe` True
        it "2147483647 is prime" $ isPrime 2147483647 `shouldBe` True
    describe "totient" $
        prop "same to naive" prop_totientSameToNaive
    describe "divisors" $
        prop "same to naive" prop_divisorsSameToNaive
    describe "withPrimes 46337 generates smallPrimes" $
        it "equal to smallPrimes" $
            withPrimes 46337 U.toList `shouldBe` smallPrimes

prop_primeFactorsFactorize :: Positive Int -> Bool
prop_primeFactorsFactorize (getPositive -> x)
    = x == product (primeFactors x)

prop_primeFactctorsFactorizeByPrime :: Positive Int -> Bool
prop_primeFactctorsFactorizeByPrime (getPositive -> x)
   = all isPrime $ primeFactors x

naiveTotient :: Int -> Int
naiveTotient x = length [i | i<-[1..x], gcd x i == 1]

prop_totientSameToNaive :: Small Word -> Bool
prop_totientSameToNaive (fromIntegral.getSmall -> x)
    = x == 0 || totient x == naiveTotient x

naiveDivisors :: Int -> [Int]
naiveDivisors x = [i | i<-[1..x], rem x i == 0]

prop_divisorsSameToNaive :: Small Word -> Bool
prop_divisorsSameToNaive (fromIntegral.getSmall -> x)
    = x == 0 || divisors x == naiveDivisors x
