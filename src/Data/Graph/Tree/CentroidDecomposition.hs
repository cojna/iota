module Data.Graph.Tree.CentroidDecomposition where

import Control.Monad
import Control.Monad.ST
import Data.Function
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

import Data.Graph.Sparse

data CentroidDecomposition = CentroidDecomposition
    { parentCentroidCD :: U.Vector Int
    , subtreeSizeCD :: U.Vector Int
    }

nothingCD :: Int
nothingCD = -1

-- | /O(1)/
memberCD ::
    CentroidDecomposition ->
    -- centroid
    Vertex ->
    Vertex ->
    Bool
memberCD CentroidDecomposition{subtreeSizeCD} centroid v =
    U.unsafeIndex subtreeSizeCD v < U.unsafeIndex subtreeSizeCD centroid
{-# INLINE memberCD #-}

centroidDecomposition :: SparseGraph w -> CentroidDecomposition
centroidDecomposition gr = runST $ do
    parent <- UM.replicate (numVerticesSG gr) nothingCD
    subtreeSize <- UM.replicate (numVerticesSG gr) (0 :: Int)
    let root = 0
    fix
        ( \dfs pv v -> do
            U.forM_ (gr `adj` v) $ \nv -> when (nv /= pv) $ do
                dfs v nv
            sz <-
                U.foldl' (+) 1
                    <$> U.mapM
                        (UM.unsafeRead subtreeSize)
                        (U.filter (/= pv) $ gr `adj` v)
            UM.unsafeWrite subtreeSize v sz
        )
        nothingCD
        root

    fix
        ( \dfs v -> do
            n <- UM.unsafeRead subtreeSize v
            sizes <- U.forM (gr `adj` v) $ \nv -> do
                (,) nv <$> UM.unsafeRead subtreeSize nv
            case U.find (\(_, sz) -> n < sz * 2 && sz < n) sizes of
                Just (nv, _) -> do
                    sznv <- UM.unsafeRead subtreeSize nv
                    szv <- UM.unsafeRead subtreeSize v
                    UM.unsafeWrite subtreeSize v $ szv - sznv
                    UM.unsafeWrite subtreeSize nv szv
                    UM.unsafeRead parent v >>= UM.unsafeWrite parent nv
                    UM.unsafeWrite parent v nothingCD
                    dfs nv
                Nothing -> do
                    -- v is centroid
                    U.forM_ (gr `adj` v) $ \nv -> do
                        sz <- UM.unsafeRead subtreeSize nv
                        when (sz < n) $ do
                            UM.unsafeWrite parent nv v
                            dfs nv
        )
        root
    CentroidDecomposition
        <$> U.unsafeFreeze parent
        <*> U.unsafeFreeze subtreeSize
