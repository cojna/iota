{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}

module Algorithm.TwoSat where

import Control.Monad.Primitive
import Control.Monad.ST
import qualified Data.Vector.Unboxed as U

import Data.Graph.Sparse
import Data.Graph.Sparse.SCC

twoSat :: Int -> (forall s. CNFBuilder s () -> ST s ()) -> Maybe (U.Vector Bool)
twoSat n run
  | satisfiable = Just $
    U.generate n $ \i ->
      U.unsafeIndex comp i > U.unsafeIndex comp (i + offset)
  | otherwise = Nothing
  where
    satisfiable = U.and $
      U.generate n $ \i ->
        U.unsafeIndex comp i /= U.unsafeIndex comp (i + offset)
    offset = n
    !comp =
      stronglyConnectedComponents $
        buildSparseGraph (2 * n) run

type CNFBuilder s w = SparseGraphBuilder s w

addClauseCNF ::
  (PrimMonad m) =>
  CNFBuilder (PrimState m) () ->
  (Int, Bool) ->
  (Int, Bool) ->
  m ()
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
