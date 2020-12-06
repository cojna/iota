{-# LANGUAGE BangPatterns, RecordWildCards, TypeApplications #-}

module Data.Graph.Tree.HLD where

import           Control.Monad
import           Control.Monad.ST
import           Data.Function
import qualified Data.Vector.Fusion.Stream.Monadic as MS
import qualified Data.Vector.Unboxed               as U
import qualified Data.Vector.Unboxed.Mutable       as UM


import           Data.Graph.Sparse
import           My.Prelude                       (stream)

type HLDIndex = Int

-- | Heavy-Light-Decomposition
data HLD = HLD
    { indexHLD    :: U.Vector HLDIndex
    , parentHLD   :: U.Vector Vertex
    , pathHeadHLD :: U.Vector Vertex
    } deriving (Show)

-- | /O(log V)/
lcaHLD :: HLD -> Vertex -> Vertex -> Vertex
lcaHLD HLD{..} u v = go u v
  where
    go !x !y
        | ix > iy = go y x
        | otherwise =
            let !hx = U.unsafeIndex pathHeadHLD x
                !hy = U.unsafeIndex pathHeadHLD y
            in if hx /= hy
               then go x $ U.unsafeIndex parentHLD hy
               else x
      where
        !ix = U.unsafeIndex indexHLD x
        !iy = U.unsafeIndex indexHLD y

-- | /O(log V)/
pathHLD :: HLD -> Vertex -> Vertex -> [(HLDIndex, HLDIndex)]
pathHLD HLD{..} = go
  where
    go !x !y
        | ix > iy = go y x
        | hx /= hy =
            let !ihy = U.unsafeIndex indexHLD hy
                !iy' = iy + 1
            in (ihy, iy') : go x (U.unsafeIndex parentHLD hy)
        | ix == iy = []
        | otherwise =
            let !ix' = ix + 1
                !iy' = iy + 1
            in [(ix', iy')]
      where
        !ix = U.unsafeIndex indexHLD x
        !iy = U.unsafeIndex indexHLD y
        hx = U.unsafeIndex pathHeadHLD x
        hy = U.unsafeIndex pathHeadHLD y

-- | /O(V)/
buildHLD :: Vertex -> SparseGraph w -> HLD
buildHLD root gr@CSR{..} | numEdgesCSR == 2 * (numVerticesCSR - 1) = runST $ do
    mindexHLD <- UM.unsafeNew numVerticesCSR
    mparentHLD <- UM.replicate numVerticesCSR nothing
    mpathHeadHLD <- UM.replicate numVerticesCSR nothing

    madjacent <- U.thaw adjacentCSR
    void $ fix (\dfs pv v -> do
        UM.write mparentHLD v pv
        (size, (_, heavyId)) <- U.foldM' (\(!sz, !mm) (ei, nv) -> do
            sz' <- dfs v nv
            return (sz + sz', max mm (sz', ei))
            ) (1, (0, nothing))
            . U.filter ((/=pv) . snd) $ gr `iadj` v
        when (heavyId /= nothing) $ do
            UM.swap madjacent heavyId (offsetCSR U.! v)
        return size
        ) nothing root
    void $ fix (\dfs i h pv v -> do
        UM.write mindexHLD v i
        UM.write mpathHeadHLD v h
        let o = offsetCSR U.! v
        nv0 <- UM.read madjacent o
        acc0 <- if nv0 /= pv then dfs (i + 1) h v nv0 else pure i
        MS.foldM' (\acc j -> do
            nv <- UM.read madjacent j
            if nv /= pv
            then dfs (acc + 1) nv v nv
            else pure acc
            ) acc0
            $ stream (o + 1) (offsetCSR U.! (v + 1))
        ) 0 root nothing root

    HLD <$> U.unsafeFreeze mindexHLD
        <*> U.unsafeFreeze mparentHLD
        <*> U.unsafeFreeze mpathHeadHLD
  where
    nothing = -1
