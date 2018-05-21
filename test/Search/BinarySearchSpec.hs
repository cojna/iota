module Search.BinarySearchSpec where

import           Data.Functor.Identity
import           GHC.Exts
import           Search.BinarySearch
import           Test.Hspec
import           Test.Hspec.QuickCheck     (prop)
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Monadic

spec :: Spec
spec = do
    describe "lowerBound" $
        prop "works" prop_lowerBound
    describe "upperBound" $
        prop "works" prop_upperBound
    describe "lowerBoundM" $
        prop "works" prop_lowerBoundM
    describe "upperBoundM" $
        prop "works" prop_upperBoundM

prop_lowerBound :: Int -> Bool
prop_lowerBound k = k == lowerBound minBound maxBound (k<=)

prop_upperBound :: Int -> Bool
prop_upperBound k = k == upperBound minBound maxBound (<=k)

prop_lowerBoundM :: Int -> Bool
prop_lowerBoundM k = (k ==) . runIdentity $ lowerBoundM minBound maxBound (pure.(k<=))

prop_upperBoundM :: Int -> Bool
prop_upperBoundM k = (k ==) . runIdentity $ upperBoundM minBound maxBound (pure.(<=k))
