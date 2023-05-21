{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Vector.Sort.QuickSpec (main, spec) where

import Control.Monad.ST
import qualified Data.List as L
import Data.Vector.Sort.Quick
import qualified Data.Vector.Unboxed as U
import Test.Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "quickSort" $
    prop "naive" prop_quickSortNaive
  describe "quickSelect" $ do
    it "quickSelect [123] 0 == 123" $
      pureQuickSelect (U.singleton 123) 0 `shouldBe` 123
    it "quickSelect [3,1,4,1,5] 0 == 1" $
      pureQuickSelect (U.fromList [3, 1, 4, 1, 5]) 0 `shouldBe` 1
    it "quickSelect [3,1,4,1,5] 1 == 1" $
      pureQuickSelect (U.fromList [3, 1, 4, 1, 5]) 1 `shouldBe` 1
    it "quickSelect [3,1,4,1,5] 2 == 3" $
      pureQuickSelect (U.fromList [3, 1, 4, 1, 5]) 2 `shouldBe` 3
    it "quickSelect [3,1,4,1,5] 3 == 4" $
      pureQuickSelect (U.fromList [3, 1, 4, 1, 5]) 3 `shouldBe` 4
    it "quickSelect [3,1,4,1,5] 4 == 5" $
      pureQuickSelect (U.fromList [3, 1, 4, 1, 5]) 4 `shouldBe` 5

pureQuickSelect :: U.Vector Int -> Int -> Int
pureQuickSelect vec k = runST $ do
  mvec <- U.thaw vec
  quickSelect mvec k

prop_quickSortNaive :: [Int] -> Bool
prop_quickSortNaive xs =
  U.modify quickSort (U.fromList xs) == U.fromList (L.sort xs)
