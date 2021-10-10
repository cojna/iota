{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}

{- |
 = Segment Tree
 == Reference
   * <https://codeforces.com/blog/entry/18051>
-}
module Data.SegTree.Primal where

import Control.Monad
import Control.Monad.Primitive
import Data.Bits
import Data.Function
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import Unsafe.Coerce

newtype SegTree mv s a = SegTree {getSegTree :: mv s a}

{- |
>>> :set -XTypeApplications
>>> import Data.Semigroup (Min)
>>> import qualified Data.Vector.Unboxed.Mutable as  UM
>>> newSegTree @(Min Int) @UM.MVector 123
-}
newSegTree ::
  (Monoid a, GM.MVector mv a, PrimMonad m) =>
  Int ->
  m (SegTree mv (PrimState m) a)
newSegTree n = SegTree <$> GM.replicate (2 * extendToPowerOfTwo n) mempty

-- | /O(n)/
buildSegTree ::
  (Monoid a, PrimMonad m, G.Vector v a) =>
  v a ->
  m (SegTree (G.Mutable v) (PrimState m) a)
buildSegTree vec = do
  let n = extendToPowerOfTwo $ G.length vec
  tree <- GM.replicate (2 * n) mempty
  G.unsafeCopy (GM.unsafeSlice n (G.length vec) tree) vec
  flip fix (n - 1) $ \loop !i -> when (i >= 1) $ do
    mappend
      <$> GM.unsafeRead tree (unsafeShiftL i 1)
      <*> GM.unsafeRead tree (unsafeShiftL i 1 .|. 1)
      >>= GM.unsafeWrite tree i
    loop (i - 1)
  return $ SegTree tree

-- | /O(1)/
readSegTree ::
  (Monoid a, PrimMonad m, GM.MVector mv a) =>
  SegTree mv (PrimState m) a ->
  Int ->
  m a
readSegTree segtree k = do
  let tree = getSegTree segtree
  let n = unsafeShiftR (GM.length tree) 1
  GM.unsafeRead tree (k + n)
{-# INLINE readSegTree #-}

pullSegTree ::
  (Monoid a, PrimMonad m, GM.MVector mv a) =>
  SegTree mv (PrimState m) a ->
  Int ->
  m ()
pullSegTree seg k = do
  let tree = getSegTree seg
  mappend
    <$> GM.unsafeRead tree (unsafeShiftL k 1)
    <*> GM.unsafeRead tree (unsafeShiftL k 1 .|. 1)
    >>= GM.unsafeWrite tree k
{-# INLINE pullSegTree #-}

-- | /O(log n)/
writeSegTree ::
  (Monoid a, PrimMonad m, GM.MVector mv a) =>
  SegTree mv (PrimState m) a ->
  Int ->
  a ->
  m ()
writeSegTree segtree k v = do
  let tree = getSegTree segtree
  let n = unsafeShiftR (GM.length tree) 1
  GM.unsafeWrite tree (k + n) v
  flip fix (unsafeShiftR (k + n) 1) $ \loop !i ->
    when (i > 0) $ do
      pullSegTree segtree i
      loop $ unsafeShiftR i 1
{-# INLINE writeSegTree #-}

-- | /O(log n)/
modifySegTree ::
  (Monoid a, PrimMonad m, GM.MVector mv a) =>
  SegTree mv (PrimState m) a ->
  (a -> a) ->
  Int ->
  m ()
modifySegTree segtree f k = do
  let tree = getSegTree segtree
  let n = unsafeShiftR (GM.length tree) 1
  GM.unsafeModify tree f (k + n)
  flip fix (unsafeShiftR (k + n) 1) $ \loop !i ->
    when (i > 0) $ do
      pullSegTree segtree i
      loop $ unsafeShiftR i 1
{-# INLINE modifySegTree #-}

{- |
mconcat[a[l],...,a[r-1]]

/O(log n)/
-}
mappendFromTo ::
  (Monoid a, PrimMonad m, GM.MVector mv a) =>
  SegTree mv (PrimState m) a ->
  Int ->
  Int ->
  m a
mappendFromTo segtree l0 r0 = do
  let tree = getSegTree segtree
  let n = unsafeShiftR (GM.length tree) 1
  fix
    ( \loop !accL !accR !l !r -> do
        if l < r
          then do
            accL' <-
              if l .&. 1 == 1
                then mappend accL <$> GM.unsafeRead tree l
                else return accL
            accR' <-
              if r .&. 1 == 1
                then flip mappend accR <$> GM.unsafeRead tree (r - 1)
                else return accR
            loop
              accL'
              accR'
              (unsafeShiftR (l + l .&. 1) 1)
              (unsafeShiftR (r - r .&. 1) 1)
          else return $! accL <> accR
    )
    mempty
    mempty
    (l0 + n)
    (r0 + n)
{-# INLINE mappendFromTo #-}

{- |
mconcat[a[0],...,a[k-1]]

/O(log n)/
-}
mappendTo ::
  (Monoid a, PrimMonad m, GM.MVector mv a) =>
  SegTree mv (PrimState m) a ->
  Int ->
  m a
mappendTo segtree = mappendFromTo segtree 0
{-# INLINE mappendTo #-}

{- |
mconcat[a[0],...,a[n-1]]

/O(1)/
-}
mappendAll ::
  (PrimMonad m, GM.MVector mv a) =>
  SegTree mv (PrimState m) a ->
  m a
mappendAll segtree = GM.unsafeRead (getSegTree segtree) 1
{-# INLINE mappendAll #-}

{- | max r s.t. f (mappendFromTo seg l r) == True

>>> :set -XTypeApplications
>>> import Data.Semigroup (Min)
>>> import qualified Data.Vector.Unboxed.Mutable as  UM
>>> seg <- newSegTree @(Min Int) @UM.MVector 10
>>> upperBoundFrom seg 0 (const True)
16
-}
upperBoundFrom ::
  (Monoid a, PrimMonad m, GM.MVector mv a) =>
  SegTree mv (PrimState m) a ->
  -- | left
  Int ->
  -- | predicate s.t. f memepty == True, monotone
  (a -> Bool) ->
  m Int
upperBoundFrom segtree l p = do
  let tree = getSegTree segtree
  let !n = unsafeShiftR (GM.length tree) 1
  violationNode <-
    fix
      ( \loopUp !acc !cur -> do
          let rightParent = unsafeShiftR cur (countTrailingZeros cur)
          !acc' <- (acc <>) <$> GM.unsafeRead tree rightParent
          if p acc'
            then do
              let !cur' = rightParent + 1
              if cur' .&. negate cur' /= cur'
                then loopUp acc' cur'
                else return Nothing
            else return $ Just (acc, rightParent)
      )
      mempty
      (l + n)
  case violationNode of
    Nothing -> return n
    Just (!acc0, !cur0) -> do
      fix
        ( \loopDown !acc !cur -> do
            if cur < n
              then do
                let !leftChild = 2 * cur
                !acc' <- (acc <>) <$> GM.unsafeRead tree leftChild
                if p acc'
                  then loopDown acc' (leftChild + 1)
                  else loopDown acc leftChild
              else return $! cur - n
        )
        acc0
        cur0
{-# INLINE upperBoundFrom #-}

-- | min l s.t. f (mappendFromTo seg l r) == True
lowerBoundTo ::
  (Monoid a, PrimMonad m, GM.MVector mv a) =>
  SegTree mv (PrimState m) a ->
  -- | right
  Int ->
  -- | predicate s.t. f memepty == True, monotone
  (a -> Bool) ->
  m Int
lowerBoundTo segtree r p = do
  let tree = getSegTree segtree
  let !n = unsafeShiftR (GM.length tree) 1
  violationNode <-
    fix
      ( \loopUp !acc !cur -> do
          let leftParent =
                case unsafeShiftR cur (countTrailingZeros (complement cur)) of
                  0 -> 1 -- cur: 2 ^ n
                  v -> v
          !acc' <- (<> acc) <$> GM.unsafeRead tree leftParent
          if p acc'
            then do
              if leftParent .&. negate leftParent /= leftParent
                then loopUp acc' (leftParent - 1)
                else return Nothing
            else return $ Just (acc, leftParent)
      )
      mempty
      (r - 1 + n)
  case violationNode of
    Nothing -> return 0
    Just (!acc0, !cur0) ->
      fix
        ( \loopDown !acc !cur -> do
            if cur < n
              then do
                let !rightChild = 2 * cur + 1
                !acc' <- (<> acc) <$> GM.unsafeRead tree rightChild
                if p acc'
                  then loopDown acc' (rightChild - 1)
                  else loopDown acc rightChild
              else return $! cur + 1 - n
        )
        acc0
        cur0
{-# INLINE lowerBoundTo #-}

{- |
>>> extendToPowerOfTwo 0
1
-}
extendToPowerOfTwo :: Int -> Int
extendToPowerOfTwo x
  | x > 1 =
    unsafeCoerce @Word @Int $
      unsafeShiftR (complement zeroBits) (countLeadingZeros (x - 1)) + 1
  | otherwise = 1
