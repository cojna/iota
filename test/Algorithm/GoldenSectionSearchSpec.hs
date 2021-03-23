module Algorithm.GoldenSectionSearchSpec where

import Algorithm.GoldenSectionSearch
import Data.Semigroup
import Test.Prelude

main :: IO ()
main = hspec spec

eps :: Double
eps = 1e-6

spec :: Spec
spec = do
  describe "phi" $ do
    it "phi = (1 + sqrt 5) / 2" $ do
      phi `shouldSatisfy` approx eps ((1 + sqrt 5) / 2)
  describe "resphi" $ do
    it "resphi = 2 - phi" $ do
      resphi `shouldSatisfy` approx eps (2 - phi)
    it "resphi = 1 / (1 + phi)" $ do
      resphi `shouldSatisfy` approx eps (1 / (1 + phi))
  describe "min1/min2" $ do
    prop "mid1 (mid1 low high) high == mid2 low high" prop_mid1mid1
    prop "mid2 low (mid2 low high) == mid1 low high" prop_mid2mid2
  describe "goldenSectionSearchMin" $ do
    it "search (x - 1) ^ 2 + 2 == Min (Arg 2 1)" $
      goldenSectionSearchMin (-10) 10 f
        `shouldSatisfy` approx eps (Arg 2.0 1.0) . getMin
  describe "goldenSectionSearchMax" $ do
    it "search - (x - 1) ^ 2 - 2 == Max (Arg (-2) 1)" $
      goldenSectionSearchMax (-10) 10 (negate . f)
        `shouldSatisfy` approx eps (Arg (-2.0) 1.0) . getMax
  describe "goldenSectionSearchMin2" $ do
    it "search (x - 1) ^ 2 + (y - 2) ^ 2 + 3 == Min (Arg 3 (1, 2))" $
      goldenSectionSearchMin2 (-10) 10 g
        `shouldSatisfy` approx eps (Arg 3.0 (1.0, 2.0)) . getMin
  describe "goldenSectionSearchMax2" $ do
    it "search -(x - 1) ^ 2 - (y - 2) ^ 2 - 3 == Max (Arg (-3) (1, 2))" $
      goldenSectionSearchMax2 (-10) 10 (\x y -> - g x y)
        `shouldSatisfy` approx eps (Arg (-3.0) (1.0, 2.0)) . getMax

f :: Double -> Double
f x = (x - 1) ^ (2 :: Int) + 2

g :: Double -> Double -> Double
g x y = (x - 1) ^ (2 :: Int) + (y - 2) ^ (2 :: Int) + 3

prop_mid1mid1 :: Double -> Double -> Bool
prop_mid1mid1 x y = approx eps (mid1 (mid1 low high) high) (mid2 low high)
  where
    low = min x y
    high = max x y

prop_mid2mid2 :: Double -> Double -> Bool
prop_mid2mid2 x y = approx eps (mid2 low (mid2 low high)) (mid1 low high)
  where
    low = min x y
    high = max x y
