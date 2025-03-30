{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Graph.Unicyclic where

import Control.Monad.ST
import Data.Function
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

import Data.Buffer
import Data.Graph.Sparse
import My.Prelude

data UnicyclicComponents w = UnicyclicComponents
  { numComponentsUC :: !Int
  -- ^ equals to the number of cycles
  , componentIDsUC :: !(U.Vector Int)
  , onCycleUC :: !(U.Vector Bool)
  , inverseUC :: !(SparseGraph w)
  }

{- |
>>> uni = buildUnicyclicComponents 5 $ U.fromList [1,2,0,0,4]
>>> numComponentsUC uni
2
>>> componentIDsUC uni
[0,0,0,0,1]
>>> onCycleUC uni
[True,True,True,False,True]
-}
buildUnicyclicComponents ::
  -- | the number of vertices
  Int ->
  -- | next
  U.Vector Int ->
  UnicyclicComponents ()
buildUnicyclicComponents n next =
  buildUnicyclicComponentsW n (U.map (,()) next)

buildUnicyclicComponentsW ::
  (U.Unbox w) =>
  -- | the number of vertices
  Int ->
  -- | (next, weight)
  U.Vector (Int, w) ->
  UnicyclicComponents w
buildUnicyclicComponentsW n next = UnicyclicComponents{..}
  where
    !gr@inverseUC =
      buildDirectedGraphW n n $
        U.imap (\v (nv, w) -> (nv, v, w)) next
    (numComponentsUC, componentIDsUC) = runST $ do
      compIDs <- UM.replicate n (-1 :: Int)
      num <-
        U.foldM'
          ( \compID root -> do
              UM.unsafeRead compIDs root >>= \case
                (-1) -> do
                  UM.unsafeWrite compIDs root compID
                  fix
                    ( \dfs v -> do
                        U.forM_ (gr `adj` v) $ \nv -> do
                          UM.unsafeRead compIDs nv >>= \case
                            (-1) -> do
                              UM.unsafeWrite compIDs nv compID
                              dfs nv
                            _ -> pure ()
                    )
                    root
                  pure (compID + 1)
                _ -> pure compID
          )
          0
          (U.elemIndices True onCycleUC)
      (num,) <$> U.unsafeFreeze compIDs
    !onCycleUC = runST $ do
      inDeg <- UM.replicate n (0 :: Int)
      U.forM_ next $ \(nv, _) -> do
        UM.unsafeModify inDeg (+ 1) nv
      leaves <- newBufferAsQueue n
      rep n $ \v -> do
        UM.unsafeRead inDeg v >>= \case
          0 -> pushBack v leaves
          _ -> pure ()
      fix $ \loop -> do
        popFront leaves >>= \case
          Nothing -> pure ()
          Just v -> do
            let (nv, _) = U.unsafeIndex next v
            UM.unsafeModify inDeg (subtract 1) nv
            UM.unsafeRead inDeg nv >>= \case
              0 -> pushBack nv leaves
              _ -> pure ()
            loop
      U.map (== 1) <$> U.unsafeFreeze inDeg
