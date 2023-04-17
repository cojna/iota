{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Math.ModulusSpec (main, spec) where

import qualified Data.List as L
import Math.Modulus
import Test.Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "powMod" $ do
    it "powMod 2 0 1000000007 = 1" $ do
      powMod @Int 2 0 1000000007 `shouldBe` 1
    it "powMod 0 0 1000000007 = 1" $ do
      powMod @Int 0 0 1000000007 `shouldBe` 1
    it "powMod 2 1000000005 1000000007 = 500000004" $ do
      powMod @Int 2 1000000005 1000000007 `shouldBe` 500000004
    it "powMod 2 (-1) 1000000007 = 500000004" $ do
      powMod @Int 2 (-1) 1000000007 `shouldBe` 500000004
    it "powMod 123456789 998244353 998244353 = 123456789" $ do
      powMod @Int 123456789 998244353 998244353 `shouldBe` 123456789
    it "powMod (-2) 2 1000000007 = 4" $ do
      powMod @Int (-2) 2 1000000007 `shouldBe` 4
    it "powMod (-2) 3 1000000007 = 999999999" $ do
      powMod @Int (-2) 3 1000000007 `shouldBe` 999999999
    prop "same to naive" prop_powModSameToNaive
    prop "Fermat's little theorem" prop_Fermat'sLittleTheorem
  describe "recipMod" $ do
    it "recipMod 2 1000000007 = 500000004" $ do
      recipMod @Int 2 1000000007 `shouldBe` 500000004
    it "recipMod 10 1000000007 = 700000005" $ do
      recipMod @Int 10 1000000007 `shouldBe` 700000005
    prop "x * recip x == 1" prop_recipMod
  describe "extGCD" $ do
    prop "gcd" prop_gcd
    prop "a * x + b * y == gcd a b" prop_bezout
    prop "|x| <= b / gcd a b, |y| <= a / gcd a b" prop_extGCD
  describe "linearDiophantine" $ do
    prop "a * x + b * y == c" prop_linearDiophantine
    prop "a * (x - k * b / g) + b * (y + k * a / g) == c" prop_linearDiophantineMultiple
  describe "crt" $ do
    it "crt (10, 20) (10, 30) = Just (10, 60)" $ do
      crt @Int (10, 20) (10, 30) `shouldBe` Just (10, 60)
    it "crt (10, 20) (10, 20) = Just (10, 20)" $ do
      crt @Int (10, 20) (10, 20) `shouldBe` Just (10, 20)
    it "crt (10, 20) (11, 20) = Nothing" $ do
      crt @Int (10, 20) (11, 20) `shouldBe` Nothing
  describe "crt'" $ do
    prop "solve equations" prop_crt'
    it "crt' (2,3) (3,5) = 8" $ do
      crt' @Int (2, 3) (3, 5) `shouldBe` 8
    it "crt' (3,5) (3,7) = 3" $ do
      crt' @Int (3, 5) (3, 7) `shouldBe` 3
    it "crt' (3,7) (3,7) = 3" $ do
      crt' @Int (3, 7) (3, 7) `shouldBe` 3
  describe "crts" $ do
    it "crts [(20,30),(30,50),(20,70)] = Just (230, 1050)" $ do
      crts @Int [(20, 30), (30, 50), (20, 70)] `shouldBe` Just (230, 1050)
    it "crts [] = Just (0, 1)" $ do
      crts @Int [] `shouldBe` Just (0, 1)
    it "crts [(1, 10), (2, 20), (3, 30)] = Nothing" $ do
      crts @Int [(1, 10), (2, 20), (3, 30)] `shouldBe` Nothing
  describe "garner" $ do
    it "garner [(2, 3), (3, 5), (2, 7)] 999 = 23" $ do
      garner @Int [(2, 3), (3, 5), (2, 7)] 999 `shouldBe` 23
    it "garner [(2, 3), (3, 5), (2, 7)] 10 = 3" $ do
      garner @Int [(2, 3), (3, 5), (2, 7)] 10 `shouldBe` 3

naivePowMod :: Integer -> Int -> Integer -> Integer
naivePowMod x n m = L.foldl' (\acc y -> acc * y `mod` m) 1 $ replicate n x

prop_powModSameToNaive :: NonNegative Integer -> Small Word -> Prime Integer -> Bool
prop_powModSameToNaive
  (getNonNegative -> x)
  (fromIntegral . getSmall -> n)
  (getPrime -> m) =
    powMod x n m == naivePowMod x n m

prop_Fermat'sLittleTheorem :: NonNegative Int -> Prime Int -> Property
prop_Fermat'sLittleTheorem
  (getNonNegative -> x)
  (getPrime -> p) =
    gcd x p == 1 ==> powMod x p p == mod x p

prop_recipMod :: Integer -> Positive Integer -> Bool
prop_recipMod
  x
  (getPositive -> m) =
    x * recipMod x m `mod` m == gcd x m `mod` m

prop_gcd :: Positive Integer -> Positive Integer -> Bool
prop_gcd
  (getPositive -> a)
  (getPositive -> b) =
    let (_, _, g) = extGCD a b
     in g == gcd a b

prop_bezout :: Positive Integer -> Positive Integer -> Bool
prop_bezout
  (getPositive -> a)
  (getPositive -> b) =
    let (x, y, _) = extGCD a b
     in a * x + b * y == gcd a b

prop_extGCD :: Positive Integer -> Positive Integer -> Bool
prop_extGCD
  (getPositive -> a)
  (getPositive -> b) =
    let (x, y, _) = extGCD a b
     in abs x <= div b (gcd a b) && abs y <= div a (gcd a b)

prop_linearDiophantine :: Integer -> Integer -> Integer -> Bool
prop_linearDiophantine a b c = case linearDiophantine a b c of
  Just (x, y) -> a * x + b * y == c
  Nothing -> True

prop_linearDiophantineMultiple :: Integer -> Integer -> Integer -> Integer -> Bool
prop_linearDiophantineMultiple a b c k = case linearDiophantine a b c of
  Just (x, y)
    | a /= 0
    , b /= 0 ->
        let x' = x - k * div b (gcd a b)
            y' = y + k * div a (gcd a b)
         in a * x' + b * y' == c
  _ -> True

-- >>> validateCRT' (0, Prime 2) (1, Prime 2)
-- False
validateCRT' :: (Int, Prime Int) -> (Int, Prime Int) -> Bool
validateCRT' (r0, p0) (r1, p1) =
  r0 == r1 || p0 /= p1

prop_crt' :: (Int, Prime Int) -> (Int, Prime Int) -> Bool
prop_crt' (r0, Prime p0) (r1, Prime p1) =
  not (validateCRT' (r0, Prime p0) (r1, Prime p1))
    || mod x p0 == mod r0 p0 && mod x p1 == mod r1 p1
  where
    x = crt' (r0, p0) (r1, p1)
