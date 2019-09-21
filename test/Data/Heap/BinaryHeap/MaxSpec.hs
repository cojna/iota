module Data.Heap.BinaryHeap.MaxSpec where

import           Control.Monad
import           Data.Function
import           Data.Heap.BinaryHeap.Max
import qualified Data.List                as L
import           Data.Primitive.MutVar
import qualified Data.Vector.Unboxed      as U
import           GHC.Exts
import           Test.Hspec
import           Test.Hspec.QuickCheck    (prop)
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

spec :: Spec
spec =
    describe "MaxHeapM" $ do
        prop "heap sort naive" prop_heapSortNaive

prop_heapSortNaive :: [Int] -> Property
prop_heapSortNaive xs = monadicIO $ do
    sorted <- run $ do
        h <- fromListM xs
        toListM h
    assert $ L.sortBy (flip compare) xs == sorted
  where
    fromListM xs = do
        h <- newBinaryHeap (length xs)
        forM_ xs $ \x ->
            insertMaxBH x h
        return h
    toListM h =
        flip fix [] $ \loop buf -> do
            top <- deleteFindMaxBH h
            case top of
                Just x  -> loop (x:buf)
                Nothing -> return $ reverse buf
