module Data.Vector.Sort.RadixSpec (main, spec) where

import qualified Data.List as L
import Data.Vector.Sort.Radix
import qualified Data.Vector.Unboxed as U
import Data.Word
import Test.Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "radixSort32" $
    prop "naive" prop_radixSort32Naive
  describe "radixSort64" $
    prop "naive" prop_radixSort64Naive

prop_radixSort32Naive :: [Word32] -> Bool
prop_radixSort32Naive xs =
  radixSort32 (U.fromList xs) == U.fromList (L.sort xs)

prop_radixSort64Naive :: [Word64] -> Bool
prop_radixSort64Naive xs =
  radixSort64 (U.fromList xs) == U.fromList (L.sort xs)
