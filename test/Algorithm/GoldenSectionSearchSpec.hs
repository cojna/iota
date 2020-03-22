module Algorithm.GoldenSectionSearchSpec where

import           Algorithm.GoldenSectionSearch
import           Data.Double.EPS
import           Test.Prelude

main :: IO ()
main = hspec spec

eq :: Double -> Double -> Bool
eq = eqEPS 1e-6

eq2 :: (Double, Double) -> (Double, Double) -> Bool
eq2 (x0, y0) (x1, y1) = eq x0 x1 && eq y0 y1

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
    describe "goldenSectionSearchDownward" $ do
        it "search (x - 1) ^ 2 + 2 == (1, 2)" $ do
            goldenSectionSearchDownward (-10) 10 (\x -> (x - 1) ^ 2 + 2)
                `shouldSatisfy` eq2 (1, 2)
    describe "goldenSectionSearchUpward" $ do
        it "search - (x - 1) ^ 2 + 2 == (1, 2)" $ do
            goldenSectionSearchUpward (-10) 10 (\x -> - (x - 1) ^ 2 + 2)
                `shouldSatisfy` eq2 (1, 2)


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
