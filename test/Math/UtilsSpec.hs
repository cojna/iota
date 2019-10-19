{-# LANGUAGE ViewPatterns #-}

module Math.UtilsSpec where

import           Test.Prelude
import           Math.Utils

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "floorSqrt" $ do
        prop "floor (sqrt x)" prop_floorSqrt

prop_floorSqrt :: NonNegative Int -> Bool
prop_floorSqrt (getNonNegative -> n) = res * res <= n && (res + 1) * (res + 1) > n
  where
    res = floorSqrt n
