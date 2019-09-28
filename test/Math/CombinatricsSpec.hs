{-# LANGUAGE ViewPatterns #-}

module Math.CombinatricsSpec (main, spec) where

import           Data.IntMod
import qualified Data.Vector.Unboxed       as U
import           Math.Combinatrics
import           Test.Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "fact" $ do
        it "fact 0 = 1" $ do
            fact 0 `shouldBe` 1
        it "fact 1 = 1" $ do
            fact 0 `shouldBe` 1
        it "fact 2 = 2" $ do
            fact 0 `shouldBe` 1
        it "fact 10 = 3628800" $ do
            fact 10 `shouldBe` 3628800
        it "fact 100 = 437918130 (mod 1000000007)" $ do
            fact 100 `shouldBe` 437918130
    describe "comb" $ do
        prop "comb n 0 = 1" prop_combN0
        prop "comb n n = 1" prop_combNN
        prop "comb n k = comb (n - 1) (k - 1) + comb (n - 1) k" $
            prop_constructPascal'sTriangle
        prop "comb n k = comb n (n - k)" prop_combSym
    describe "fact/recipFact cache" $ do
        it "fact * recipFact = 1" $
            U.zipWith (*) factCache recipFactCache
                `shouldSatisfy` U.all (== 1)

normalize :: Int -> Int
normalize = flip mod factCacheSize

prop_combN0 :: NonNegative Int -> Bool
prop_combN0 (normalize.getNonNegative -> n)
    = comb n 0 == 1

prop_combNN :: NonNegative Int -> Bool
prop_combNN (normalize.getNonNegative -> n)
    = comb n n == 1

prop_constructPascal'sTriangle
    :: Positive Int -> Positive Int -> Bool
prop_constructPascal'sTriangle
    (normalize.getPositive -> x)
    (normalize.getPositive -> y)
    = not (n - 1 >= k) || comb n k == comb (n - 1) (k - 1) + comb (n - 1) k
  where
    n = max 1 $ max x y
    k = max 1 $ min x y

prop_combSym
    :: Positive Int -> Positive Int -> Bool
prop_combSym
    (normalize.getPositive -> x)
    (normalize.getPositive -> y)
    = comb n k == comb n (n - k)
  where
    n = max x y
    k = min x y
