module Data.MinHeapMSpec where

import           Control.Monad
import           Data.Function
import           Data.HeapM
import qualified Data.List               as L
import           Data.MinHeapM
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
        prop "heap sort" prop_heapSort

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

prop_heapSort :: [Int] -> Property
prop_heapSort xs = monadicIO $ do
    sorted <- run $ do
        h <- fromListM xs
        toListM h
    assert $ L.sort xs == sorted
  where
    fromListM xs = do
        let v = U.fromList xs
        ref <- newMutVar $ U.length v
        mv <- U.unsafeThaw v
        heapify compare mv
        return $ MinHeapM ref mv
    toListM h =
        flip fix [] $ \loop buf -> do
            top <- _HMdeleteFindMinM h
            case top of
                Just x  -> loop (x:buf)
                Nothing -> return $ reverse buf
