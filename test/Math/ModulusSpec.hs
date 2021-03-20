{-# LANGUAGE BangPatterns, ViewPatterns #-}

module Math.ModulusSpec (main, spec) where

import           Data.Int
import           Data.IntMod
import qualified Data.List    as L
import           Data.Word
import           Math.Modulus
import           Test.Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "powMod" $ do
        it "powMod 2 0 1000000007 = 1" $ do
            powMod 2 0 1000000007 `shouldBe` 1
        it "powMod 0 0 1000000007 = 1" $ do
            powMod 0 0 1000000007 `shouldBe` 1
        it "powMod 2 1000000005 1000000007 = 500000004" $ do
            powMod 2 1000000005 1000000007 `shouldBe` 500000004
        it "powMod 2 (-1) 1000000007 = 500000004" $ do
            powMod 2 (-1) 1000000007 `shouldBe` 500000004
        it "powMod 123456789 998244353 998244353 = 123456789" $ do
            powMod 123456789 998244353 998244353 `shouldBe` 123456789
        it "powMod (-2) 2 1000000007 = 4" $ do
            powMod (-2) 2 1000000007 `shouldBe` 4
        it "powMod (-2) 3 1000000007 = 999999999" $ do
            powMod (-2) 3 1000000007 `shouldBe` 999999999
        prop "same to naive" prop_powModSameToNaive
        prop "Fermat's little theorem" prop_Fermat'sLittleTheorem
    describe "recipMod" $ do
        it "recipMod 2 1000000007 = 500000004" $ do
            recipMod 2 1000000007 `shouldBe` 500000004
        it "recipMod 10 1000000007 = 700000005" $ do
            recipMod 10 1000000007 `shouldBe` 700000005
        prop "x * recip x == 1" prop_recipMod
    describe "crt" $ do
        it "crt (10, 20) (10, 30) = Just (10, 60)" $ do
            crt (10, 20) (10, 30) `shouldBe` Just (10, 60)
        it "crt (10, 20) (10, 20) = Just (10, 20)" $ do
            crt (10, 20) (10, 20) `shouldBe` Just (10, 20)
        it "crt (10, 20) (11, 20) = Nothing" $ do
            crt (10, 20) (11, 20) `shouldBe` Nothing
    describe "crt'" $ do
        prop "solve equations" prop_crt'
        it "crt' (2,3) (3,5) = 8" $ do
            crt' (2,3) (3,5) `shouldBe` 8
        it "crt' (3,5) (3,7) = 3" $ do
            crt' (3,5) (3,7) `shouldBe` 3
        it "crt' (3,7) (3,7) = 3" $ do
            crt' (3,7) (3,7) `shouldBe` 3
    describe "crts" $ do
        it "crts [(20,30),(30,50),(20,70)] = Just (230, 1050)" $ do
            crts [(20,30),(30,50),(20,70)] `shouldBe` Just (230, 1050)
        it "crts [] = Just (0, 1)" $ do
            crts [] `shouldBe` Just (0, 1)
        it "crts [(1, 10), (2, 20), (3, 30)] = Nothing" $ do
            crts [(1, 10), (2, 20), (3, 30)] `shouldBe` Nothing
    describe "garner" $ do
        it "garner [(2, 3), (3, 5), (2, 7)] 999 = 23" $ do
            garner [(2, 3), (3, 5), (2, 7)] 999 `shouldBe` 23
        it "garner [(2, 3), (3, 5), (2, 7)] 10 = 3" $ do
            garner [(2, 3), (3, 5), (2, 7)] 10 `shouldBe` 3

naivePowMod :: Integer -> Int -> Integer -> Integer
naivePowMod x n m = L.foldl' (\acc x -> acc * x `mod` m) 1 $ replicate n x

prop_powModSameToNaive :: NonNegative Integer -> Small Word -> Prime Integer -> Bool
prop_powModSameToNaive
    (getNonNegative -> x)
    (fromIntegral.getSmall -> n)
    (getPrime -> m)
    = powMod x n m == naivePowMod x n m

prop_Fermat'sLittleTheorem :: NonNegative Int -> Prime Int -> Property
prop_Fermat'sLittleTheorem
    (getNonNegative -> x) (getPrime -> p)
    = gcd x p == 1 ==> powMod x p p == mod x p

prop_recipMod :: Positive Integer -> Prime Integer -> Bool
prop_recipMod
    (getPositive -> x)
    (getPrime -> m)
    = x `mod` m == 0 || x * recipMod x m `mod` m == 1

-- >>> validateCRT' (0, Prime 2) (1, Prime 2)
-- False
validateCRT' :: (Int, Prime Int) -> (Int, Prime Int) -> Bool
validateCRT' (r0, p0) (r1, p1)
    = r0 == r1 || p0 /= p1

prop_crt' :: (Int, Prime Int) -> (Int, Prime Int) -> Bool
prop_crt' (r0, Prime p0) (r1, Prime p1)
    = not (validateCRT' (r0, Prime p0) (r1, Prime p1))
    || mod x p0 == mod r0 p0 && mod x p1 == mod r1 p1
  where
    x = crt' (r0, p0) (r1, p1)
