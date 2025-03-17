{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Graph.Tree.LCT where

import Control.Monad
import Control.Monad.Primitive
import Data.Coerce
import Data.Function
import Data.Int
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

-- | for commutative monoids
data LCT s a = LCT
  { parentLCT :: UM.MVector s Int32
  , leftChildLCT :: UM.MVector s Int32
  , rightChildLCT :: UM.MVector s Int32
  , commMonoidLCT :: UM.MVector s a
  , foldSubtreesLCT :: UM.MVector s a
  , lazyRevFlagLCT :: UM.MVector s Bool
  }

newLCT :: (U.Unbox a, Monoid a, PrimMonad m) => Int -> m (LCT (PrimState m) a)
newLCT n =
  LCT
    <$> UM.replicate n (fromIntegral nothingLCT)
    <*> UM.replicate n (fromIntegral nothingLCT)
    <*> UM.replicate n (fromIntegral nothingLCT)
    <*> UM.replicate n mempty
    <*> UM.replicate n mempty
    <*> UM.replicate n False

buildLCT :: (U.Unbox a, Monoid a, PrimMonad m) => U.Vector a -> m (LCT (PrimState m) a)
buildLCT vs =
  LCT
    <$> UM.replicate n (fromIntegral nothingLCT)
    <*> UM.replicate n (fromIntegral nothingLCT)
    <*> UM.replicate n (fromIntegral nothingLCT)
    <*> U.thaw vs
    <*> U.thaw vs
    <*> UM.replicate n False
  where
    n = U.length vs

{- | make v root

>>> lct <- newLCT @() 3
>>> linkLCT lct 1 0 >> linkLCT lct 2 1
>>> findRootLCT lct 2
0
>>> evertLCT lct 1
>>> findRootLCT lct 2
1
-}
evertLCT :: (U.Unbox a, Monoid a, PrimMonad m) => LCT (PrimState m) a -> Int -> m ()
evertLCT lct@LCT{..} v = do
  void $ exposeLCT lct (asSplayNodeId v)
  UM.unsafeWrite lazyRevFlagLCT v True
  pushLCT lct (asSplayNodeId v)
{-# INLINE evertLCT #-}

-- | require: the edge @(u, v)@ exists
cutLCT :: (U.Unbox a, Monoid a, PrimMonad m) => LCT (PrimState m) a -> Int -> Int -> m ()
cutLCT lct u v = do
  evertLCT lct u
  void $ exposeLCT lct (asSplayNodeId v)
  -- u is left child of v
  setLeftChildLCT lct (asSplayNodeId v) nothingLCT
  setParentLCT lct (asSplayNodeId u) nothingLCT
  pullLCT lct (asSplayNodeId v)
{-# INLINE cutLCT #-}

{- | link u to v

require: @u@ and @v@ are *not connected*
-}
linkLCT :: (U.Unbox a, Monoid a, PrimMonad m) => LCT (PrimState m) a -> Int -> Int -> m ()
linkLCT lct u v = do
  evertLCT lct u
  void $ exposeLCT lct (asSplayNodeId v)
  setParentLCT lct (asSplayNodeId u) (asSplayNodeId v)
{-# INLINE linkLCT #-}

{- | require: @l@ and @r@ connected

>>> import Data.Monoid
>>> lct <- buildLCT @(Sum Int) $ U.fromList $ map Sum [0..3]
>>> linkLCT lct 1 0 >> linkLCT lct 2 1 >> linkLCT lct 3 1
>>> mconcatPathLCT lct 0 1
Sum {getSum = 1}
>>> mconcatPathLCT lct 2 3  -- 2 - 1 - 3
Sum {getSum = 6}
>>> mconcatPathLCT lct 0 3  -- 0 - 1 - 3
Sum {getSum = 4}
-}
mconcatPathLCT :: (U.Unbox a, Monoid a, PrimMonad m) => LCT (PrimState m) a -> Int -> Int -> m a
mconcatPathLCT lct@LCT{..} l r = do
  evertLCT lct l
  void $ exposeLCT lct (asSplayNodeId r)
  UM.unsafeRead foldSubtreesLCT r
{-# INLINE mconcatPathLCT #-}

-- | root is the left most node of the root path
findRootLCT :: (U.Unbox a, Monoid a, PrimMonad m) => LCT (PrimState m) a -> Int -> m Int
findRootLCT lct v0 = do
  u0 <- exposeLCT lct (asSplayNodeId v0)
  lu0 <- getLeftChildLCT lct u0
  if lu0 == nothingLCT
    then pure $ coerce @SplayNodeId u0
    else
      fix
        ( \loop !v -> do
            pushLCT lct v
            lv <- getLeftChildLCT lct v
            if lv /= nothingLCT
              then loop lv
              else pure $ coerce @SplayNodeId v
        )
        lu0
{-# INLINE findRootLCT #-}

{- | commutative monoids

>>> import Data.Monoid
>>> lct <- buildLCT @(Sum Int) $ U.fromList $ map Sum [0..2]
>>> linkLCT lct 1 0 >> linkLCT lct 2 1
>>> mconcatPathLCT lct 0 2
Sum {getSum = 3}
>>> setCMonLCT lct 1 (Sum 100)
>>> mconcatPathLCT lct 0 2
Sum {getSum = 102}
-}
setCMonLCT :: (U.Unbox a, Monoid a, PrimMonad m) => LCT (PrimState m) a -> Int -> a -> m ()
setCMonLCT lct@LCT{..} v x = do
  traverseDownLCT (pushLCT lct) lct (asSplayNodeId v)
  splayLCT lct (asSplayNodeId v)
  UM.unsafeWrite commMonoidLCT v x
  pullLCT lct (asSplayNodeId v)
{-# INLINE setCMonLCT #-}

{- | require: u and v are connected

>>> lct <- newLCT @() 4
>>> linkLCT lct 1 0 >> linkLCT lct 2 1 >> linkLCT lct 3 2
>>> evertLCT lct 0
>>> lcaLCT lct 0 3
0
>>> evertLCT lct 2
>>> lcaLCT lct 0 3
2
-}
lcaLCT :: (U.Unbox a, Monoid a, PrimMonad m) => LCT (PrimState m) a -> Int -> Int -> m Int
lcaLCT t u v = do
  void $ exposeLCT t (asSplayNodeId u)
  coerce @SplayNodeId <$> exposeLCT t (asSplayNodeId v)
{-# INLINE lcaLCT #-}

newtype SplayNodeId = SplayNodeId {getSplayNodeId :: Int}
  deriving newtype (Eq, Ord, Show, Num, Real, Enum, Integral)

asSplayNodeId :: Int -> SplayNodeId
asSplayNodeId = coerce
{-# INLINE asSplayNodeId #-}

getLeftChildLCT ::
  (PrimMonad m) =>
  LCT (PrimState m) a ->
  SplayNodeId ->
  m SplayNodeId
getLeftChildLCT LCT{leftChildLCT} v =
  fromIntegral <$> UM.unsafeRead leftChildLCT (coerce v)
{-# INLINE getLeftChildLCT #-}

getRightChildLCT ::
  (PrimMonad m) =>
  LCT (PrimState m) a ->
  SplayNodeId ->
  m SplayNodeId
getRightChildLCT LCT{rightChildLCT} v =
  fromIntegral <$> UM.unsafeRead rightChildLCT (coerce v)
{-# INLINE getRightChildLCT #-}

getParentLCT ::
  (PrimMonad m) =>
  LCT (PrimState m) a ->
  SplayNodeId ->
  m SplayNodeId
getParentLCT LCT{parentLCT} v = fromIntegral <$> UM.unsafeRead parentLCT (coerce v)
{-# INLINE getParentLCT #-}

setLeftChildLCT ::
  (PrimMonad m) =>
  LCT (PrimState m) a ->
  SplayNodeId ->
  SplayNodeId ->
  m ()
setLeftChildLCT LCT{leftChildLCT} v lv =
  UM.unsafeWrite leftChildLCT (coerce v) (fromIntegral @Int $ coerce lv)
{-# INLINE setLeftChildLCT #-}

setRightChildLCT ::
  (PrimMonad m) =>
  LCT (PrimState m) a ->
  SplayNodeId ->
  SplayNodeId ->
  m ()
setRightChildLCT LCT{rightChildLCT} v rv =
  UM.unsafeWrite rightChildLCT (coerce v) (fromIntegral @Int $ coerce rv)
{-# INLINE setRightChildLCT #-}

setParentLCT ::
  (PrimMonad m) =>
  LCT (PrimState m) a ->
  SplayNodeId ->
  SplayNodeId ->
  m ()
setParentLCT LCT{parentLCT} v pv =
  UM.unsafeWrite parentLCT (coerce v) (fromIntegral @Int $ coerce pv)
{-# INLINE setParentLCT #-}

nothingLCT :: SplayNodeId
nothingLCT = SplayNodeId (-1)

isSplayTreeRootLCT :: (PrimMonad m) => LCT (PrimState m) a -> SplayNodeId -> m Bool
isSplayTreeRootLCT lct v = do
  pv <- getParentLCT lct v
  if pv == nothingLCT
    then pure True
    else do
      lpv <- getLeftChildLCT lct pv
      rpv <- getRightChildLCT lct pv
      pure $! lpv /= v && rpv /= v
{-# INLINE isSplayTreeRootLCT #-}

pullLCT :: (U.Unbox a, Monoid a, PrimMonad m) => LCT (PrimState m) a -> SplayNodeId -> m ()
pullLCT lct@LCT{..} v = do
  lv <- getLeftChildLCT lct v
  mlv <-
    if lv /= nothingLCT
      then UM.unsafeRead foldSubtreesLCT $ coerce @SplayNodeId lv
      else pure mempty
  rv <- getRightChildLCT lct v
  mrv <-
    if rv /= nothingLCT
      then UM.unsafeRead foldSubtreesLCT $ coerce @SplayNodeId rv
      else pure mempty
  mv <- UM.unsafeRead commMonoidLCT $ coerce @SplayNodeId v
  UM.unsafeWrite foldSubtreesLCT (coerce @SplayNodeId v) $ mlv <> mv <> mrv
{-# INLINE pullLCT #-}

pushLCT :: (U.Unbox a, Monoid a, PrimMonad m) => LCT (PrimState m) a -> SplayNodeId -> m ()
pushLCT lct@LCT{..} v = do
  UM.unsafeRead lazyRevFlagLCT (coerce @SplayNodeId v) >>= \case
    False -> pure ()
    True -> do
      UM.unsafeWrite lazyRevFlagLCT (coerce @SplayNodeId v) False
      lv <- getLeftChildLCT lct v
      rv <- getRightChildLCT lct v
      setLeftChildLCT lct v rv
      setRightChildLCT lct v lv
      when (lv /= nothingLCT) $ do
        UM.unsafeModify lazyRevFlagLCT not (coerce @SplayNodeId lv)
      when (rv /= nothingLCT) $ do
        UM.unsafeModify lazyRevFlagLCT not (coerce @SplayNodeId rv)
{-# INLINE pushLCT #-}

-- | from the splay tree root to v
traverseDownLCT ::
  (U.Unbox a, Monoid a, PrimMonad m) =>
  (SplayNodeId -> m ()) ->
  LCT (PrimState m) a ->
  SplayNodeId ->
  m ()
traverseDownLCT f lct = fix $ \goUp v -> do
  isSplayTreeRootLCT lct v >>= \case
    True -> f v
    False -> do
      pv <- getParentLCT lct v
      goUp pv
      f v
{-# INLINE traverseDownLCT #-}

{-
     pv           v
    /  \         / \
   v   rpv ==> lv  pv
  / \              / \
lv  rv           rv  rpv
-}
rotateRightLCT ::
  (U.Unbox a, Monoid a, PrimMonad m) =>
  LCT (PrimState m) a ->
  -- | has pasrent node
  SplayNodeId ->
  m ()
rotateRightLCT lct v = do
  pv <- getParentLCT lct v
  ppv <- getParentLCT lct pv
  setParentLCT lct v ppv
  when (ppv /= nothingLCT) $ do
    lppv <- getLeftChildLCT lct ppv
    when (lppv == pv) $ do
      setLeftChildLCT lct ppv v
    rppv <- getRightChildLCT lct ppv
    when (rppv == pv) $ do
      setRightChildLCT lct ppv v
  setParentLCT lct pv v
  rv <- getRightChildLCT lct v
  setLeftChildLCT lct pv rv
  setRightChildLCT lct v pv
  when (rv /= nothingLCT) $ do
    setParentLCT lct rv pv
  pullLCT lct pv
  pullLCT lct v

{-
{-# INLINE rotateRightLCT #-}
<no location info>: error:
    Simplifier ticks exhausted
-}

{-
   pv         v
  /  \       / \
lpv  v  ==> pv  rv
    / \    /  \
  lv  rv  lpv  lv
-}
rotateLeftLCT ::
  (U.Unbox a, Monoid a, PrimMonad m) =>
  LCT (PrimState m) a ->
  -- | has parent node
  SplayNodeId ->
  m ()
rotateLeftLCT lct v = do
  pv <- getParentLCT lct v
  ppv <- getParentLCT lct pv
  setParentLCT lct v ppv
  when (ppv /= nothingLCT) $ do
    lppv <- getLeftChildLCT lct ppv
    when (lppv == pv) $ do
      setLeftChildLCT lct ppv v
    rppv <- getRightChildLCT lct ppv
    when (rppv == pv) $ do
      setRightChildLCT lct ppv v
  setParentLCT lct pv v
  lv <- getLeftChildLCT lct v
  setRightChildLCT lct pv lv
  setLeftChildLCT lct v pv
  when (lv /= nothingLCT) $ do
    setParentLCT lct lv pv
  pullLCT lct pv
  pullLCT lct v

{-
-- {-# INLINE rotateLeftLCT #-}
<no location info>: error:
    Simplifier ticks exhausted
-}

splayLCT ::
  (U.Unbox a, Monoid a, PrimMonad m) =>
  LCT (PrimState m) a ->
  SplayNodeId ->
  m ()
splayLCT lct v =
  fix $ \loop -> do
    isRoot <- isSplayTreeRootLCT lct v
    unless isRoot $ do
      pv <- getParentLCT lct v
      ppv <- getParentLCT lct pv
      isRoot' <- isSplayTreeRootLCT lct pv
      if isRoot'
        then do
          lpv <- getLeftChildLCT lct pv
          if lpv == v
            then rotateRightLCT lct v
            else rotateLeftLCT lct v
        else do
          lpv <- getLeftChildLCT lct pv
          lppv <- getLeftChildLCT lct ppv
          case (lppv == pv, lpv == v) of
            (True, True) -> do
              rotateRightLCT lct pv
              rotateRightLCT lct v
            (True, False) -> do
              rotateLeftLCT lct v
              rotateRightLCT lct v
            (False, True) -> do
              rotateRightLCT lct v
              rotateLeftLCT lct v
            (False, False) -> do
              rotateLeftLCT lct pv
              rotateLeftLCT lct v
      loop
{-# INLINE splayLCT #-}

{- |
make v on the root path

properties
* @v@ is the root of the splay tree.
* @v@ is the right most node of the root path.
* @expose u >> expose v == lca u v@.

>>> lct <- newLCT @() 2
>>> linkLCT lct 0 1
>>> evertLCT lct 0
>>> exposeLCT lct 1
1
>>> isSplayTreeRootLCT lct 1
True
>>> findRootLCT lct 1
0
-}
exposeLCT ::
  (U.Unbox a, Monoid a, PrimMonad m) =>
  LCT (PrimState m) a ->
  SplayNodeId ->
  m SplayNodeId
exposeLCT lct v0 = do
  fix
    ( \goUp !v !rv ->
        if v /= nothingLCT
          then do
            traverseDownLCT (pushLCT lct) lct v
            splayLCT lct v
            setRightChildLCT lct v rv
            pullLCT lct v
            pv <- getParentLCT lct v
            goUp pv v
          else do
            -- rv is the root of splay tree
            splayLCT lct v0
            return rv
    )
    v0
    nothingLCT
{-# INLINE exposeLCT #-}
