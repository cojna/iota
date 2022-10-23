{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Graph.Dense where

import Control.Monad.ST
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

data DenseGraph a = DenseGraph
  { numVerticesDG :: !Int
  , adjacentDG :: !(U.Vector a)
  }

instance (U.Unbox a, Show a) => Show (DenseGraph a) where
  show = show . toListDG

indexDG :: Int -> Int -> Int -> Int
indexDG n i j = n * i + j
{-# INLINE indexDG #-}

matDG :: (U.Unbox a) => DenseGraph a -> Int -> Int -> a
matDG gr i j = U.unsafeIndex (adjacentDG gr) (indexDG (numVerticesDG gr) i j)
{-# INLINE matDG #-}

adjDG :: (U.Unbox a) => DenseGraph a -> Int -> U.Vector a
adjDG gr i =
  U.unsafeSlice
    (i * numVerticesDG gr)
    (numVerticesDG gr)
    (adjacentDG gr)
{-# INLINE adjDG #-}

createDenseGraph :: (U.Unbox a) => Int -> (forall s. UM.MVector s a -> ST s ()) -> DenseGraph a
createDenseGraph n f = runST $ do
  buf <- UM.unsafeNew (n * n)
  f buf
  DenseGraph n <$> U.unsafeFreeze buf

fromListNDG :: (U.Unbox a) => Int -> [[a]] -> DenseGraph a
fromListNDG n xss =
  DenseGraph
    { numVerticesDG = n
    , adjacentDG = U.fromListN (n * n) $ concat xss
    }

fromListDG :: (U.Unbox a) => [[a]] -> DenseGraph a
fromListDG = length >>= fromListNDG

toListDG :: (U.Unbox a) => DenseGraph a -> [[a]]
toListDG gr = [U.toList $ adjDG gr i | i <- [0 .. numVerticesDG gr - 1]]
