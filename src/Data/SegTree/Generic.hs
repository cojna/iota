{-# LANGUAGE BangPatterns, LambdaCase, TypeApplications #-}

-- |
-- = Segment Tree
-- == Reference
--   * <https://codeforces.com/blog/entry/18051>

module Data.SegTree.Generic where

import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Data.Bits
import           Data.Function
import           Data.Monoid
import qualified Data.Vector.Generic               as G
import qualified Data.Vector.Generic.Mutable       as GM
import           Data.Word
import           Unsafe.Coerce

newtype SegTree mv s a = SegTree { getSegTree :: mv s a }

newSegTree
    :: (Monoid a, GM.MVector mv a, PrimMonad m)
    => Int -> m (SegTree mv (PrimState m) a)
newSegTree n = SegTree <$> GM.replicate (2 * extendToPowerOfTwo n) mempty

-- | /O(n)/
buildSegTree
    :: (Monoid a, PrimMonad m, G.Vector v a)
    => v a -> m (SegTree (G.Mutable v) (PrimState m) a)
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
readSegTree :: (Monoid a, PrimMonad m, GM.MVector mv a)
    => SegTree mv (PrimState m) a -> Int -> m a
readSegTree segtree k = do
    let tree = getSegTree segtree
    let n = unsafeShiftR (GM.length tree) 1
    GM.unsafeRead tree (k + n)
{-# INLINE readSegTree #-}

pullSegTree :: (Monoid a, PrimMonad m, GM.MVector mv a)
    => SegTree mv (PrimState m) a -> Int -> m ()
pullSegTree seg k = do
    let tree = getSegTree seg
    mappend
        <$> GM.unsafeRead tree (unsafeShiftL k 1)
        <*> GM.unsafeRead tree (unsafeShiftL k 1 .|. 1)
        >>= GM.unsafeWrite tree k
{-# INLINE pullSegTree #-}

-- | /O(log n)/
writeSegTree
    :: (Monoid a, PrimMonad m, GM.MVector mv a)
    => SegTree mv (PrimState m) a -> Int -> a -> m ()
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
modifySegTree
    :: (Monoid a, PrimMonad m, GM.MVector mv a)
    => SegTree mv (PrimState m) a -> (a -> a) -> Int -> m ()
modifySegTree segtree f k = do
    let tree = getSegTree segtree
    let n = unsafeShiftR (GM.length tree) 1
    GM.unsafeModify tree f (k + n)
    flip fix (unsafeShiftR (k + n) 1) $ \loop !i ->
        when (i > 0) $ do
            pullSegTree segtree i
            loop $ unsafeShiftR i 1
{-# INLINE modifySegTree #-}

-- | mappend [l..r)
--
-- /O(log n)/
mappendFromTo
    :: (Monoid a, PrimMonad m, GM.MVector mv a)
    => SegTree mv (PrimState m) a -> Int -> Int -> m a
mappendFromTo segtree l r = do
    let tree = getSegTree segtree
    let n = unsafeShiftR (GM.length tree) 1
    fix (\loop !accL !accR !l !r -> do
        if l < r
        then do
            accL' <- if l .&. 1 ==  1
                then mappend accL <$> GM.unsafeRead tree l
                else return accL
            accR' <- if r .&. 1 == 1
                then flip mappend accR <$> GM.unsafeRead tree (r - 1)
                else return accR
            loop accL' accR'
                (unsafeShiftR (l + l .&. 1) 1)
                (unsafeShiftR (r - r .&. 1) 1)
        else return $! accL <> accR
        ) mempty mempty (l + n) (r + n)
{-# INLINE mappendFromTo #-}

-- | mappend [0..k)
--
-- /O(log n)/
mappendTo
    :: (Monoid a, PrimMonad m, GM.MVector mv a)
    => SegTree mv (PrimState m) a -> Int -> m a
mappendTo segtree = mappendFromTo segtree 0
{-# INLINE mappendTo #-}

-- | mappend [0..n)
--
-- /O(1)/
mappendAll
    :: (PrimMonad m, GM.MVector mv a)
    => SegTree mv (PrimState m) a -> m a
mappendAll segtree = GM.unsafeRead (getSegTree segtree) 1
{-# INLINE mappendAll #-}


maxRightSegTree :: (Monoid a, PrimMonad m, GM.MVector mv a)
    => SegTree mv (PrimState m) a -> Int -> (a -> Bool) -> m Int
maxRightSegTree segtree l p = do
    let tree = getSegTree segtree
    let !n = unsafeShiftR (GM.length tree) 1
    fix ( \oloop !oacc !ol -> do
        let ol' = unsafeShiftR ol (countTrailingZeros ol)
        oacc' <- (<> oacc) <$> GM.unsafeRead tree ol'
        if p oacc'
        then do
            let !ol'' = ol' + 1
            if (ol'' .&. (-ol'')) /= ol''
            then oloop oacc' ol''
            else return $! n
        else do
            fix (\iloop !iacc !il -> do
                    if il < n
                    then do
                        let il' = 2 * il
                        iacc' <- (<> iacc) <$> GM.unsafeRead tree il'
                        if p iacc'
                        then iloop iacc' (il' + 1)
                        else iloop iacc il'
                    else return $! il - n
                ) oacc ol'
        ) mempty (l + n)
{-# INLINE maxRightSegTree #-}

minLeftSegTree :: (Monoid a, PrimMonad m, GM.MVector mv a)
    => SegTree mv (PrimState m) a -> Int -> (a -> Bool) -> m Int
minLeftSegTree segtree r p = do
    let tree = getSegTree segtree
    let !n = unsafeShiftR (GM.length tree) 1
    fix ( \oloop !oacc !or -> do
        let or' = fix (\loop !r ->
                if r > 1 && r .&. 1 == 1
                then loop (unsafeShiftR r 1)
                else r
                ) or
        oacc' <- (<> oacc) <$> GM.unsafeRead tree or'
        if p oacc'
        then do
            if (or' .&. (-or')) /= or'
            then oloop oacc' or'
            else return 0
        else do
            fix (\iloop !iacc !ir -> do
                    if ir < n
                    then do
                        let ir' = 2 * ir + 1
                        iacc' <- (<> iacc) <$> GM.unsafeRead tree ir'
                        if p iacc'
                        then iloop iacc' (ir' - 1)
                        else iloop iacc ir'
                    else return $! ir + 1 - n
                ) oacc or'
        ) mempty (r + n)
{-# INLINE minLeftSegTree #-}

-- |
-- >>> extendToPowerOfTwo 0
-- 1
extendToPowerOfTwo :: Int -> Int
extendToPowerOfTwo x
    | x > 1
        = unsafeCoerce @Word @Int
        $ unsafeShiftR (complement zeroBits) (countLeadingZeros (x - 1)) + 1
    | otherwise = 1
