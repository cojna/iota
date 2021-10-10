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

-- | max r s.t. f (mappendFromTo seg l r) == True
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
  fix
    ( \oloop !oacc !ol -> do
        let ol' = unsafeShiftR ol (countTrailingZeros ol)
        oacc' <- (<> oacc) <$> GM.unsafeRead tree ol'
        if p oacc'
          then do
            let !ol'' = ol' + 1
            if (ol'' .&. (- ol'')) /= ol''
              then oloop oacc' ol''
              else return $! n
          else do
            fix
              ( \iloop !iacc !il -> do
                  if il < n
                    then do
                      let il' = 2 * il
                      iacc' <- (<> iacc) <$> GM.unsafeRead tree il'
                      if p iacc'
                        then iloop iacc' (il' + 1)
                        else iloop iacc il'
                    else return $! il - n
              )
              oacc
              ol'
    )
    mempty
    (l + n)
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
lowerBoundTo segtree r0 p = do
  let tree = getSegTree segtree
  let !n = unsafeShiftR (GM.length tree) 1
  fix
    ( \loop !acc !r -> do
        let r' =
              fix
                ( \iloop !ir ->
                    if ir > 1 && ir .&. 1 == 1
                      then iloop (unsafeShiftR ir 1)
                      else ir
                )
                (r - 1)
        acc' <- (<> acc) <$> GM.unsafeRead tree r'
        if p acc'
          then do
            if (r' .&. (- r')) /= r'
              then loop acc' r'
              else return 0
          else do
            fix
              ( \iloop !iacc !ir -> do
                  if ir < n
                    then do
                      let ir' = 2 * ir + 1
                      iacc' <- (<> iacc) <$> GM.unsafeRead tree ir'
                      if p iacc'
                        then iloop iacc' (ir' - 1)
                        else iloop iacc ir'
                    else return $! ir + 1 - n
              )
              acc
              r'
    )
    mempty
    (r0 + n)
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
