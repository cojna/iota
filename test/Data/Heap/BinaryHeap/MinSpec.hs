module Data.Heap.BinaryHeap.MinSpec where

import           Control.Monad
import           Data.Function
import           Data.Heap.BinaryHeap.Min
import qualified Data.List               as L
import           Data.Primitive.MutVar
import qualified Data.Vector.Unboxed     as U
import           GHC.Exts
import           Test.Hspec
import           Test.Hspec.QuickCheck   (prop)
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

spec :: Spec
spec =
    describe "MinHeapM" $ do
        prop "heap sort naive" prop_heapSortNaive

prop_heapSortNaive :: [Int] -> Property
prop_heapSortNaive xs = monadicIO $ do
    sorted <- run $ do
        h <- fromListM xs
        toListM h
    assert $ L.sort xs == sorted
  where
    fromListM xs = do
        h <- _HMnewHeap (length xs)
        forM_ xs $ \x ->
            _HMinsertM x h
        return h
    toListM h =
        flip fix [] $ \loop buf -> do
            top <- _HMdeleteFindMinM h
            case top of
                Just x  -> loop (x:buf)
                Nothing -> return $ reverse buf

