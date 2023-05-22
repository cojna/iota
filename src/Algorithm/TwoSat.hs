{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}

module Algorithm.TwoSat where

import Control.Monad.Primitive
import Control.Monad.ST
import qualified Data.Vector.Unboxed as U

import Data.Graph.Sparse
import Data.Graph.Sparse.SCC

{- | 2-SAT

@(a0 \\\/ a1) \/\\ not a0@

>>> twoSat 2 2 (\b -> addClauseCNF b (0, True) (1, True) >> addClauseCNF b (0, False) (0, False))
Just [False,True]

@a0 \/\\ not a1 \/\\ (not a0 \\\/ a1)@

>>> print $ twoSat 2 3 (\b -> addClauseCNF b (0, True) (0, True) >> addClauseCNF b (1, False) (1, False) >> addClauseCNF b (0, False) (1, True))
Nothing

@a0 \\\/ not a0@

>>> twoSat 1 1 (\b -> addClauseCNF b (0, True) (0, False))
Just [True]
-}
twoSat ::
  -- | the number of variables
  Int ->
  -- | upper bound on the number of clauses
  Int ->
  -- | CNF(Conjunctive Normal Form)
  (forall s. CNFBuilder s () -> ST s ()) ->
  Maybe (U.Vector Bool)
twoSat numVars numClauses run
  | satisfiable = Just $
      U.generate numVars $ \i ->
        U.unsafeIndex comp i > U.unsafeIndex comp (i + offset)
  | otherwise = Nothing
  where
    satisfiable = U.and $
      U.generate numVars $ \i ->
        U.unsafeIndex comp i /= U.unsafeIndex comp (i + offset)
    offset = numVars
    !comp =
      stronglyConnectedComponents $
        buildSparseGraph (2 * numVars) (2 * numClauses) run

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
