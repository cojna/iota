{-# LANGUAGE BangPatterns, CPP, LambdaCase, RecordWildCards #-}

module Data.Graph.MaxFlow where

import           Control.Monad
import           Control.Monad.Primitive
import           Data.Function
import           Data.Primitive.MutVar
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM

import           Data.VecQueue

nothing :: Int
nothing = -1

inf :: (Num a) => a
inf = 0x3f3f3f3f3f3f
{-# INLINE inf #-}

type Vertex = Int

data MaxFlow s cap = MaxFlow
    { numVerticesMF :: !Int
    , numEdgesMF    :: !Int
    , offsetMF      :: U.Vector Int
    , dstMF         :: U.Vector Vertex
    , residualMF    :: UM.MVector s cap
    , levelMF       :: UM.MVector s Int
    , revEdgeMF     :: U.Vector Int
    , iterMF        :: UM.MVector s Int
    , queueMF       :: VecQueue s Vertex
    }

-- | Dinic O(V^2E)
runMaxFlow :: (U.Unbox cap, Num cap, Ord cap, PrimMonad m)
    => Vertex -> Vertex -> MaxFlow (PrimState m) cap -> m cap
runMaxFlow src sink mf@MaxFlow{..} = do
    flip fix 0 $ \loopBFS !flow -> do
        UM.set levelMF nothing
        clearVQ queueMF
        bfsMF src mf
        lsink <- UM.unsafeRead levelMF sink
        if lsink == nothing
        then return flow
        else do
            U.unsafeCopy iterMF offsetMF
            flip fix flow $ \loopDFS !f -> do
                df <- dfsMF src sink inf mf
                if df > 0
                then loopDFS (f + df)
                else loopBFS f


bfsMF :: (Num cap, Ord cap, U.Unbox cap, PrimMonad m)
    => Vertex -> MaxFlow (PrimState m) cap -> m ()
bfsMF src MaxFlow{..} = do
    UM.unsafeWrite levelMF src 0
    enqueue src queueMF
    fix $ \loop -> do
        dequeue queueMF >>= \case
            Just v -> do
                let start = U.unsafeIndex offsetMF v
                let end = U.unsafeIndex offsetMF (v + 1)
                U.forM_ (U.generate (end - start) (+start)) $ \e -> do
                    let nv = U.unsafeIndex dstMF e
                    res <- UM.unsafeRead residualMF e
                    lnv <- UM.unsafeRead levelMF nv
                    when (res > 0 && lnv == nothing) $ do
                        UM.unsafeRead levelMF v >>= UM.unsafeWrite levelMF nv . (+1)
                        enqueue nv queueMF
                    loop
            Nothing -> return ()
{-# INLINE bfsMF #-}

dfsMF :: (U.Unbox cap, Num cap, Ord cap, PrimMonad m)
    => Vertex -> Vertex -> cap -> MaxFlow (PrimState m) cap -> m cap
dfsMF v0 sink flow0 MaxFlow{..} = dfs v0 flow0 return
  where
    dfs !v !flow k
        | v == sink = k flow
        | otherwise = fix $ \loop -> do
            e <- UM.unsafeRead iterMF v
            if e < U.unsafeIndex offsetMF (v + 1)
            then do
                UM.unsafeWrite iterMF v (e + 1)
                let nv = U.unsafeIndex dstMF e
                cap <- UM.unsafeRead residualMF e
                lv <- UM.unsafeRead levelMF v
                lnv <- UM.unsafeRead levelMF nv
                if cap > 0 && lv < lnv
                then do
                    dfs nv (min flow cap) $ \f -> do
                        if f > 0
                        then do
                            UM.unsafeModify residualMF (subtract f) e
                            UM.unsafeModify residualMF (+f)
                                (U.unsafeIndex revEdgeMF e)
                            k f
                        else loop
                else loop
            else k 0
{-# INLINE dfsMF #-}

data MaxFlowBuilder s cap = MaxFlowBuilder
    { numVerticesMFB :: !Int
    , inDegreeMFB    :: UM.MVector s Int
    , edgesMFB       :: MutVar s [(Vertex, Vertex, cap)]
    }

newMaxFlowBuilder :: (PrimMonad m)
    => Int -> m (MaxFlowBuilder (PrimState m) cap)
newMaxFlowBuilder n = MaxFlowBuilder n
    <$> UM.replicate n 0
    <*> newMutVar []

buildMaxFlowBuilder :: (PrimMonad m)
    => Int -> [(Vertex, Vertex, cap)] -> m (MaxFlowBuilder (PrimState m) cap)
buildMaxFlowBuilder n edges = do
    mfb <- newMaxFlowBuilder n
    forM_ edges $ \(src, dst, cap) -> do
        addEdgeMFB src dst cap mfb
    return mfb

addEdgeMFB :: (PrimMonad m)
    => Vertex -> Vertex -> cap -> MaxFlowBuilder (PrimState m) cap -> m ()
addEdgeMFB !src !dst !cap MaxFlowBuilder{..} = do
    UM.unsafeModify inDegreeMFB (+1) src
    UM.unsafeModify inDegreeMFB (+1) dst
    modifyMutVar' edgesMFB ((src, dst, cap):)
{-# INLINE addEdgeMFB #-}

buildMaxFlow :: (Num cap, U.Unbox cap, PrimMonad m)
    => MaxFlowBuilder (PrimState m) cap -> m (MaxFlow (PrimState m) cap)
buildMaxFlow MaxFlowBuilder{..} = do
    offsetMF <- U.scanl' (+) 0 <$> U.unsafeFreeze inDegreeMFB
    let numVerticesMF = numVerticesMFB
    let numEdgesMF = U.last offsetMF

    moffset <- U.thaw offsetMF
    mdstMF <- UM.replicate numEdgesMF nothing
    mrevEdgeMF <- UM.replicate numEdgesMF nothing
    residualMF <- UM.replicate numEdgesMF 0

    edges <- readMutVar edgesMFB
    forM_ edges $ \(src, dst, cap) -> do
        srcOffset <- UM.unsafeRead moffset src
        dstOffset <- UM.unsafeRead moffset dst
        UM.unsafeModify moffset (+1) src
        UM.unsafeModify moffset (+1) dst
        UM.unsafeWrite mdstMF srcOffset dst
        UM.unsafeWrite mdstMF dstOffset src
        UM.unsafeWrite mrevEdgeMF srcOffset dstOffset
        UM.unsafeWrite mrevEdgeMF dstOffset srcOffset
        UM.unsafeWrite residualMF srcOffset cap

    dstMF <- U.unsafeFreeze mdstMF
    levelMF <- UM.replicate numVerticesMF nothing
    revEdgeMF <- U.unsafeFreeze mrevEdgeMF
    iterMF <- UM.replicate numVerticesMF 0
    U.unsafeCopy iterMF offsetMF
    queueMF <- newVecQueue numVerticesMF
    return MaxFlow{..}
