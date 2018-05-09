module Data.QueueSpec where

import           Data.List
import           Data.Queue
import           Test.Hspec
import           Test.Hspec.QuickCheck     (prop)
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary

spec :: Spec
spec =
    describe "Queue" $
        prop "First In First Out" prop_fifo

prop_fifo :: [Int] -> Bool
prop_fifo xs = xs == unfoldr _Qhead q
  where
    q = foldl _Qsnoc _Qempty xs
