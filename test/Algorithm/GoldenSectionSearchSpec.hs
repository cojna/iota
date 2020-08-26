module Algorithm.GoldenSectionSearchSpec where

import           Algorithm.GoldenSectionSearch
import           Data.EPS
import           Data.Semigroup
import           Test.Prelude hiding (Arg)

main :: IO ()
main = hspec spec

eq :: Double -> Double -> Bool
eq = eqEPS 1e-6

eq2 :: (Double, Double) -> (Double, Double) -> Bool
eq2 (x0, y0) (x1, y1) = eq x0 x1 && eq y0 y1

eq3 :: (Double, Double, Double) -> (Double, Double, Double) -> Bool
eq3 (x0, y0, z0) (x1, y1, z1) = eq x0 x1 && eq y0 y1 && eq z0 z1

eqArg :: Arg Double Double -> Arg Double Double -> Bool
eqArg (Arg y0 x0) (Arg y1 x1) = eq2 (x0, y0) (x1, y1)

eqArg2 :: Arg Double (Double, Double) -> Arg Double (Double, Double) -> Bool
eqArg2 (Arg z0 (x0, y0)) (Arg z1 (x1, y1)) = eq3 (x0, y0, z0) (x1, y1, z1)

spec :: Spec
spec = do
    describe "phi" $ do
        it "phi = (1 + sqrt 5) / 2" $ do
            phi `shouldSatisfy` eq ((1 + sqrt 5) / 2)
    describe "resphi" $ do
        it "resphi = 2 - phi" $ do
            resphi `shouldSatisfy` eq (2 - phi)
        it "resphi = 1 / (1 + phi)" $ do
            resphi `shouldSatisfy` eq (1 / (1 + phi))
    describe "min1/min2" $ do
        prop "mid1 (mid1 low high) high == mid2 low high" prop_mid1mid1
        prop "mid2 low (mid2 low high) == mid1 low high" prop_mid2mid2
    describe "goldenSectionSearchMin" $ do
        it "search (x - 1) ^ 2 + 2 == Min (Arg 2 1)" $
            goldenSectionSearchMin (-10) 10 f
                `shouldSatisfy` eqArg (Arg 2.0 1.0) . getMin
    describe "goldenSectionSearchMax" $ do
        it "search - (x - 1) ^ 2 - 2 == Max (Arg (-2) 1)" $
            goldenSectionSearchMax (-10) 10 (negate . f)
                `shouldSatisfy` eqArg (Arg (-2.0) 1.0) . getMax
    describe "goldenSectionSearchMin2" $ do
        it "search (x - 1) ^ 2 + (y - 2) ^ 2 + 3 == Min (Arg 3 (1, 2))" $
            goldenSectionSearchMin2 (-10) 10 g
                `shouldSatisfy` eqArg2 (Arg 3.0 (1.0, 2.0)) . getMin
    describe "goldenSectionSearchMax2" $ do
        it "search -(x - 1) ^ 2 - (y - 2) ^ 2 - 3 == Max (Arg (-3) (1, 2))" $
            goldenSectionSearchMax2 (-10) 10 (\x y -> -g x y)
                `shouldSatisfy` eqArg2 (Arg (-3.0) (1.0, 2.0)) . getMax

f :: Double -> Double
f x = (x - 1) ^ 2 + 2

g :: Double -> Double -> Double
g x y = (x - 1) ^ 2 + (y - 2) ^ 2 + 3

prop_mid1mid1 :: Double -> Double -> Bool
prop_mid1mid1 x y = mid1 (mid1 low high) high `eq` mid2 low high
  where
    low = min x y
    high = max x y

prop_mid2mid2 :: Double -> Double -> Bool
prop_mid2mid2 x y = mid2 low (mid2 low high) `eq` mid1 low high
  where
    low = min x y
    high = max x y
