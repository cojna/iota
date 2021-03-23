{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Graph.Tree.Rerooting where

import Data.Function
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

--
import Data.Graph.Sparse

rerootingDP ::
  forall w a m.
  (U.Unbox w, U.Unbox a, U.Unbox m, Monoid m) =>
  SparseGraph w ->
  (a -> m) ->
  (Vertex -> m -> a) ->
  U.Vector a
rerootingDP gr toM foldChildren = dp2
  where
    root :: Vertex
    root = 0

    dp1 :: U.Vector a
    !dp1 = U.create $ do
      dp <- UM.unsafeNew (numVerticesCSR gr)
      _ <-
        fix
          ( \dfs p v -> do
              res <-
                foldChildren v . U.foldl' (<>) mempty . U.map toM
                  <$> U.mapM (dfs v) (U.filter (/= p) $ gr `adj` v)
              UM.write dp v res
              return res
          )
          (-1)
          root
      return dp

    dp2 :: U.Vector a
    dp2 = U.create $ do
      dp <- UM.unsafeNew (numVerticesCSR gr)
      UM.write dp root (dp1 U.! root)
      fix
        ( \dfs parent !fromParent v -> do
            let children = U.filter (/= parent) $ gr `adj` v
            UM.write dp v
              . foldChildren v
              . U.foldl' (<>) fromParent
              . U.map toM
              $ U.backpermute dp1 children
            let cumL =
                  U.prescanl' (<>) mempty . U.map toM $
                    U.backpermute dp1 children
            let cumR =
                  U.prescanr' (<>) mempty . U.map toM $
                    U.backpermute dp1 children
            U.zipWithM_
              (dfs v . toM . foldChildren v . (<> fromParent))
              (U.zipWith (<>) cumL cumR)
              children
        )
        (-1)
        mempty
        root
      return dp
