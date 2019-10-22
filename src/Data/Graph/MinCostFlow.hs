{-# LANGUAGE BangPatterns, LambdaCase, RecordWildCards #-}

module Data.Graph.MinCostFlow where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Primitive
import           Data.Bits
import           Data.Function
import           Data.Primitive.MutVar
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM
import           Data.Word
import           Unsafe.Coerce

import           Data.Heap.Binary
import           Utils                       (rep)

nothing :: Int
nothing = -1

inf :: Int
inf = 0x3f3f3f3f3f3f

type Vertex = Int
type Cost = Int
type Capacity = Int

data MinCostFlow s = MinCostFlow
    { numVerticesMCF :: !Int
    , numEdgesMCF    :: !Int
    , offsetMCF      :: U.Vector Int
    , dstMCF         :: U.Vector Vertex
    , costMCF        :: U.Vector Cost
    , residualMCF    :: UM.MVector s Capacity
    , potentialMCF   :: UM.MVector s Cost
    , distMCF        :: UM.MVector s Cost
    , heapMCF        :: MinBinaryHeap s Word64 -- (Cost, Vertex)
    , revEdgeMCF     :: U.Vector Int
    , prevVertexMCF  :: UM.MVector s Vertex
    , prevEdgeMCF    :: UM.MVector s Int
    }
-- | Primal Dual O(FElog V)
runMinCostFlow :: (PrimMonad m)
    => Vertex -> Vertex -> Capacity -> MinCostFlow (PrimState m) -> m (Maybe Cost)
runMinCostFlow source sink flow mcf@MinCostFlow{..} = go 0 flow
  where
    go !res !f
        | f == 0 = return $ Just res
        | otherwise = do
            canFlow <- dijkstraMCF source sink mcf
            if canFlow
            then do
                rep numVerticesMCF $ \v -> do
                    dv <- UM.unsafeRead distMCF v
                    UM.unsafeModify potentialMCF (+dv) v
                flowed <- updateResidualMCF sink f mcf
                hsink <- UM.unsafeRead potentialMCF sink
                go (hsink * flowed + res) (f - flowed)
            else return Nothing

encodeMCF :: Cost -> Vertex -> Word64
encodeMCF cost v = unsafeCoerce $ unsafeShiftL cost 16 .|. v
{-# INLINE encodeMCF #-}

decodeMCF :: Word64 -> (Cost, Vertex)
decodeMCF costv = unsafeCoerce (cost, v)
  where
    !cost = unsafeShiftR costv 16
    !v = costv .&. 0xffff
{-# INLINE decodeMCF #-}

dijkstraMCF :: (PrimMonad m)
    => Vertex -> Vertex -> MinCostFlow (PrimState m) -> m Bool
dijkstraMCF source sink MinCostFlow{..} = do
    UM.set distMCF inf
    UM.unsafeWrite distMCF source 0
    clearBH heapMCF
    insertBH (encodeMCF 0 source) heapMCF

    fix $ \loop -> do
        deleteFindTopBH heapMCF >>= \case
            Just cv -> do
                let (c, v) = decodeMCF cv
                dv <- UM.unsafeRead distMCF v
                unless (c > dv) $ do
                    let start = U.unsafeIndex offsetMCF v
                    let end   = U.unsafeIndex offsetMCF (v + 1)
                    U.forM_ (U.generate (end - start) (+start)) $ \e -> do
                        let nv = U.unsafeIndex dstMCF e
                        let v2nv = U.unsafeIndex costMCF e
                        cap  <- UM.unsafeRead residualMCF e
                        hv   <- UM.unsafeRead potentialMCF v
                        hnv  <- UM.unsafeRead potentialMCF nv
                        old  <- UM.unsafeRead distMCF nv
                        let dnv = dv + v2nv + hv - hnv
                        when (cap > 0 && dnv < old) $ do
                            UM.unsafeWrite distMCF nv dnv
                            UM.unsafeWrite prevVertexMCF nv v
                            UM.unsafeWrite prevEdgeMCF nv e
                            insertBH (encodeMCF dnv nv) heapMCF
                loop
            Nothing -> do
                cost <- UM.unsafeRead distMCF sink
                return $! cost < inf
{-# INLINE dijkstraMCF #-}

updateResidualMCF :: (PrimMonad m)
    => Vertex -> Capacity -> MinCostFlow (PrimState m) -> m Capacity
updateResidualMCF sink flow MinCostFlow{..} = go sink flow return
  where
    go !v !f k = do
        pv <- UM.unsafeRead prevVertexMCF v
        if pv < 0
        then k f
        else do
            pv2v <- UM.unsafeRead prevEdgeMCF v
            f' <- UM.unsafeRead residualMCF pv2v
            go pv (min f f') $ \nf -> do
                UM.unsafeModify residualMCF (subtract nf) pv2v
                UM.unsafeModify residualMCF (+nf) (U.unsafeIndex revEdgeMCF pv2v)
                k nf
{-# INLINE updateResidualMCF #-}


data MinCostFlowBuilder s = MinCostFlowBuilder
    { numVerticesMCFB :: !Int
    , inDegreeMCFB    :: UM.MVector s Int
    , edgesMCFB       :: MutVar s [(Vertex, Vertex, Cost, Capacity)]
    }

newMinCostFlowBuilder :: (PrimMonad m)
    => Int -> m (MinCostFlowBuilder (PrimState m))
newMinCostFlowBuilder n = MinCostFlowBuilder n
    <$> UM.replicate n 0
    <*> newMutVar []

buildMinCostFlowBuilder :: (PrimMonad m)
    => Int -> [(Vertex, Vertex, Cost, Capacity)]
    -> m (MinCostFlowBuilder (PrimState m))
buildMinCostFlowBuilder n edges = do
    mcfb <- newMinCostFlowBuilder n
    forM_ edges $ \(src, dst, cost, capacity) -> do
        addEdgeMCFB src dst cost capacity mcfb
    return mcfb

addEdgeMCFB :: (PrimMonad m)
    => Vertex -> Vertex -> Cost -> Capacity
    -> MinCostFlowBuilder (PrimState m) -> m ()
addEdgeMCFB src dst cost capacity MinCostFlowBuilder{..}
    = assert (cost >= 0) $ do
        UM.unsafeModify inDegreeMCFB (+1) src
        UM.unsafeModify inDegreeMCFB (+1) dst
        modifyMutVar' edgesMCFB ((src, dst, cost, capacity):)

buildMinCostFlow :: (PrimMonad m)
    => MinCostFlowBuilder (PrimState m) -> m (MinCostFlow (PrimState m))
buildMinCostFlow MinCostFlowBuilder{..} = do
    offsetMCF <- U.scanl' (+) 0 <$> U.unsafeFreeze inDegreeMCFB
    let numVerticesMCF = numVerticesMCFB
    let numEdgesMCF = U.last offsetMCF

    moffset <- U.thaw offsetMCF
    edges <- readMutVar edgesMCFB
    mdstMCF <- UM.replicate numEdgesMCF nothing
    mcostMCF <- UM.replicate numEdgesMCF 0
    mrevEdgeMCF <- UM.replicate numEdgesMCF nothing
    residualMCF <- UM.replicate numEdgesMCF 0

    forM_ edges $ \(src, dst, cost, capacity) -> do
        srcOffset <- UM.unsafeRead moffset src
        dstOffset <- UM.unsafeRead moffset dst
        UM.unsafeModify moffset (+1) src
        UM.unsafeModify moffset (+1) dst

        UM.unsafeWrite mdstMCF srcOffset dst
        UM.unsafeWrite mdstMCF dstOffset src
        UM.unsafeWrite mcostMCF srcOffset cost
        UM.unsafeWrite mcostMCF dstOffset (-cost)
        UM.unsafeWrite mrevEdgeMCF srcOffset dstOffset
        UM.unsafeWrite mrevEdgeMCF dstOffset srcOffset
        UM.unsafeWrite residualMCF srcOffset capacity

    dstMCF <- U.unsafeFreeze mdstMCF
    costMCF <- U.unsafeFreeze mcostMCF
    potentialMCF <- UM.replicate numVerticesMCF 0
    distMCF <- UM.replicate numVerticesMCF 0
    heapMCF <- newMinBinaryHeap numEdgesMCF
    revEdgeMCF <- U.unsafeFreeze mrevEdgeMCF
    prevVertexMCF <- UM.replicate numVerticesMCF nothing
    prevEdgeMCF <- UM.replicate numVerticesMCF nothing
    return MinCostFlow{..}
