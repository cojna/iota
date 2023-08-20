{-# LANGUAGE LambdaCase #-}

module Data.Graph.Sparse.TopSort where

import Control.Monad.ST
import Data.Function
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

import Data.Buffer
import Data.Graph.Sparse

{- |
>>> topSort $ buildDirectedGraph 4 4 $ U.fromList [(0,1),(0,2),(1,3),(2,3)]
Just [0,1,2,3]
>>> topSort $ buildDirectedGraph 2 0 U.empty
Just [0,1]
>>> topSort $ buildDirectedGraph 2 2 $ U.fromList [(0,1),(1,0)]
Nothing
-}
topSort :: SparseGraph w -> Maybe (U.Vector Int)
topSort gr = runST $ do
  let n = numVerticesSG gr
  q <- newBufferAsQueue n
  let inDegree =
        U.unsafeAccumulate (+) (U.replicate n (0 :: Int))
          . U.map (flip (,) 1)
          $ adjacentSG gr
  U.mapM_ (flip pushBack q . fst)
    . U.filter ((== 0) . snd)
    $ U.indexed inDegree
  inDeg <- U.unsafeThaw inDegree
  fix $ \loop -> do
    popFront q >>= \case
      Just v -> do
        U.forM_ (gr `adj` v) $ \u -> do
          UM.unsafeRead inDeg u >>= \case
            1 -> pushBack u q
            i -> UM.unsafeWrite inDeg u (i - 1)
        loop
      Nothing -> return ()
  buf <- unsafeFreezeInternalBuffer q
  if U.length buf == n
    then return $ Just buf
    else return Nothing
