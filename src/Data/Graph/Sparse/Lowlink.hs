{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Graph.Sparse.Lowlink where

import Control.Monad
import Control.Monad.ST
import Data.Function
import Data.Primitive
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

import Data.Graph.Sparse
import My.Prelude (rep)

data Lowlink = Lowlink
  { lowlinkLL :: U.Vector Int
  , preordLL :: U.Vector Int
  , parentLL :: U.Vector Vertex
  }
  deriving (Show)

buildLowlink :: SparseGraph w -> Lowlink
buildLowlink gr = runST $ do
  let numV = numVerticesCSR gr
  low <- UM.replicate numV nothing
  preord <- UM.replicate numV nothing
  parent <- UM.replicate numV nothing
  vars <- UM.replicate numVars 0

  rep numV $ \root -> do
    ordRoot <- UM.unsafeRead preord root
    when (ordRoot == nothing) $ do
      fix
        ( \dfs pv v -> do
            ordV <- UM.unsafeRead vars _preordId
            UM.unsafeWrite vars _preordId (ordV + 1)

            UM.unsafeWrite preord v ordV
            UM.unsafeWrite low v ordV
            UM.unsafeWrite parent v pv

            U.forM_ (gr `adj` v) $ \u -> do
              ordU <- UM.unsafeRead preord u
              if ordU == nothing
                then do
                  dfs v u
                  lowU <- UM.unsafeRead low u
                  UM.unsafeModify low (min lowU) v
                else when (u /= pv) $ do
                  UM.unsafeModify low (min ordU) v
        )
        nothing
        root
  Lowlink <$> U.unsafeFreeze low
    <*> U.unsafeFreeze preord
    <*> U.unsafeFreeze parent
  where
    nothing = -1
    numVars = 1
    _preordId = 0

articulations :: SparseGraph w -> U.Vector Vertex
articulations gr = U.findIndices id isArticulation
  where
    nothing = -1
    !Lowlink{..} = buildLowlink gr
    !isArticulation = U.create $ do
      isa <- UM.replicate (numVerticesCSR gr) False
      flip U.imapM parentLL $ \v pv -> when (pv /= nothing) $ do
        when (U.unsafeIndex preordLL pv <= U.unsafeIndex lowlinkLL v) $ do
          UM.unsafeWrite isa pv True
      U.forM_ (U.findIndices (== nothing) parentLL) $ \root -> do
        UM.unsafeWrite isa root
          . (>= 2)
          . U.length
          . U.filter (\v -> U.unsafeIndex parentLL v == root)
          $ gr `adj` root
      return isa

bridges :: SparseGraph w -> U.Vector Edge
bridges gr = U.imapMaybe isBridge parentLL
  where
    nothing = -1
    !Lowlink{..} = buildLowlink gr
    isBridge v pv
      | pv /= nothing
        , U.unsafeIndex preordLL pv < U.unsafeIndex lowlinkLL v =
        Just (pv, v)
      | otherwise = Nothing
