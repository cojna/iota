{-# LANGUAGE BangPatterns, LambdaCase #-}

-- |
-- = Segment Tree
-- == Reference
--   * <https://codeforces.com/blog/entry/18051>

module Data.SegTree where

import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Data.Bits
import           Data.Function
import           Data.Monoid
import qualified Data.Vector                 as V
import qualified Data.Vector.Mutable         as VM
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM
import           Data.Word
--
import           Utils                       (unsafeShiftRL)

newtype SegTree s a = SegTree { getSegTree :: UM.MVector s a }

newSegTree
    :: (Monoid a, U.Unbox a, PrimMonad m)
    => Int -> m (SegTree (PrimState m) a)
newSegTree n = SegTree <$> UM.replicate (2 * extendToPowerOfTwo n) mempty

-- | /O(n)/
buildSegTree
    :: (Monoid a, U.Unbox a, PrimMonad m)
    => U.Vector a -> m (SegTree (PrimState m) a)
buildSegTree vec = do
    let n = extendToPowerOfTwo $ U.length vec
    tree <- UM.replicate (2 * n) mempty
    U.unsafeCopy (UM.unsafeSlice n (U.length vec) tree) vec
    U.forM_ (U.iterateN (n - 1) (subtract 1) (n - 1)) $ \i -> do
        x <- mappend
            <$> UM.unsafeRead tree (unsafeShiftL i 1)
            <*> UM.unsafeRead tree (unsafeShiftL i 1 .|. 1)
        UM.unsafeWrite tree i x
    return $ SegTree tree

-- | /O(1)/
readSegTree :: (Monoid a, U.Unbox a, PrimMonad m)
    => SegTree (PrimState m) a -> Int -> m a
readSegTree segtree k = do
    let tree = getSegTree segtree
    let n = unsafeShiftRL (UM.length tree) 1
    UM.unsafeRead tree (k + n)

-- | /O(log n)/
writeSegTree
    :: (Monoid a, U.Unbox a, PrimMonad m)
    => SegTree (PrimState m) a -> Int -> a -> m ()
writeSegTree segtree k v = do
    let tree = getSegTree segtree
    let n = unsafeShiftRL (UM.length tree) 1
    UM.unsafeWrite tree (k + n) v
    flip fix (k + n) $ \loop !i ->
        when (i > 1) $ do
            x <- mappend
                <$> UM.unsafeRead tree i
                <*> UM.unsafeRead tree (i `xor` 1)
            UM.unsafeWrite tree (unsafeShiftRL i 1) x
            loop $ unsafeShiftR i 1

-- | /O(log n)/
modifySegTree
    :: (Monoid a, U.Unbox a, PrimMonad m)
    => SegTree (PrimState m) a -> (a -> a) -> Int -> m ()
modifySegTree segtree f k = do
    let tree = getSegTree segtree
    let n = unsafeShiftRL (UM.length tree) 1
    UM.unsafeModify tree f (k + n)
    flip fix (k + n) $ \loop !i ->
        when (i > 1) $ do
            x <- mappend
                <$> UM.unsafeRead tree i
                <*> UM.unsafeRead tree (i `xor` 1)
            UM.unsafeWrite tree (unsafeShiftRL i 1) x
            loop $ unsafeShiftR i 1

-- | mappend [l..r)
--
-- /O(log n)/
mappendFromTo
    :: (Monoid a, U.Unbox a, PrimMonad m)
    => SegTree (PrimState m) a -> Int -> Int -> m a
mappendFromTo segtree l r = do
    let tree = getSegTree segtree
    let n = unsafeShiftRL (UM.length tree) 1
    let stepL l
            | l .&. 1 == 1 = \acc ->
                mappend acc <$> UM.unsafeRead tree l
            | otherwise = return

        stepR r
            | r .&. 1 == 1 = \acc ->
                flip mappend acc <$> UM.unsafeRead tree (r - 1)
            | otherwise = return

        go l r k
            | l < r
                = go (unsafeShiftRL (l + l .&. 1) 1)
                     (unsafeShiftRL (r - r .&. 1) 1)
                $ stepL l >=> (stepR r >=> k)
            | otherwise = k
    go (n + l) (n + r) return mempty

-- | mappend [0..k)
--
-- /O(log n)/
mappendTo
    :: (Monoid a, U.Unbox a, PrimMonad m)
    => SegTree (PrimState m) a -> Int -> m a
mappendTo segtree = mappendFromTo segtree 0
{-# INLINE mappendTo #-}

-- | mappend [0..n)
--
-- /O(1)/
mappendAll
    :: (U.Unbox a, PrimMonad m)
    => SegTree (PrimState m) a -> m a
mappendAll segtree = UM.unsafeRead (getSegTree segtree) 1
{-# INLINE mappendAll #-}


maxRightSegTree :: (Monoid a, U.Unbox a, PrimMonad m)
    => SegTree (PrimState m) a -> Int -> (a -> Bool) -> m Int
maxRightSegTree segtree l p = do
    let tree = getSegTree segtree
    let !n = unsafeShiftRL (UM.length tree) 1
    fix ( \oloop !oacc !ol -> do
        let ol' = unsafeShiftRL ol (countTrailingZeros ol)
        oacc' <- (<> oacc) <$> UM.unsafeRead tree ol'
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
                        iacc' <- (<> iacc) <$> UM.unsafeRead tree il'
                        if p iacc'
                        then iloop iacc' (il' + 1)
                        else iloop iacc il'
                    else return $! il - n
                ) oacc ol'
        ) mempty (l + n)
{-# INLINE maxRightSegTree #-}

minLeftSegTree :: (Monoid a, U.Unbox a, PrimMonad m)
    => SegTree (PrimState m) a -> Int -> (a -> Bool) -> m Int
minLeftSegTree segtree r p = do
    let tree = getSegTree segtree
    let !n = unsafeShiftRL (UM.length tree) 1
    fix ( \oloop !oacc !or -> do
        let or' = fix (\loop !r ->
                if r > 1 && r .&. 1 == 1
                then loop (unsafeShiftRL r 1)
                else r
                ) or
        oacc' <- (<> oacc) <$> UM.unsafeRead tree or'
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
                        iacc' <- (<> iacc) <$> UM.unsafeRead tree ir'
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
    | x > 1 = unsafeShiftRL (-1) (countLeadingZeros (x - 1)) + 1
    | otherwise = 1
