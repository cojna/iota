{-# LANGUAGE BangPatterns, RankNTypes #-}

module Algorithm.TwoSat where

import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM

import           Data.Graph.Sparse
import           Data.Graph.Sparse.SCC

twoSat :: Int -> (forall s.CNFBuilder s () -> ST s ()) -> Maybe (U.Vector Bool)
twoSat n run
    | satisfiable = Just $ U.generate n $ \i ->
        U.unsafeIndex comp i > U.unsafeIndex comp (i + offset)
    | otherwise = Nothing
  where
    satisfiable = U.and $ U.generate n $ \i ->
        U.unsafeIndex comp i /= U.unsafeIndex comp (i + offset)
    offset = n
    scc = stronglyConnectedComponents
        $ buildSparseGraph (2 * n) run
    !comp = U.create $ do
        cs <- UM.unsafeNew (2 * n)
        forM_ (zip[(0::Int)..]scc) $ \(i, c) -> do
            U.forM_ c $ \x -> UM.unsafeWrite cs x i
        return cs

type CNFBuilder s w = SparseGraphBuilder s w

addClauseCNF :: (PrimMonad m)
    => CNFBuilder (PrimState m) () -> (Int, Bool) -> (Int, Bool) -> m ()
addClauseCNF builder (i, f) (j, g) = do
    let !offset = quot (numVerticesSGB builder) 2
    case (f, g) of
        (True, True) -> do
            addDirectedEdge_ builder (i + offset, j)
            addDirectedEdge_ builder (j + offset, i)
        (True, False) -> do
            addDirectedEdge_ builder (i + offset, j + offset)
            addDirectedEdge_ builder (j, i)
        (False, True) -> do
            addDirectedEdge_ builder (i, j)
            addDirectedEdge_ builder (j + offset, i + offset)
        (False, False) -> do
            addDirectedEdge_ builder (i, j + offset)
            addDirectedEdge_ builder (j, i + offset)
