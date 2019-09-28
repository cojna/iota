module Data.QueueSpec (main, spec) where

import           Data.List
import           Data.Queue
import           Test.Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "Queue" $
        prop "First In First Out" prop_fifo

prop_fifo :: [Int] -> Bool
prop_fifo xs = xs == unfoldr _Qhead q
  where
    q = foldl _Qsnoc _Qempty xs
