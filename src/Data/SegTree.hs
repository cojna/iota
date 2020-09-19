{-# LANGUAGE BangPatterns, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications                                       #-}

module Data.SegTree.Lazy where

import           Control.Monad
import           Control.Monad.Primitive
import           Data.Bits
import           Data.Function
import qualified Data.Vector.Fusion.Stream.Monadic as MS
import qualified Data.Vector.Unboxed               as U
import qualified Data.Vector.Unboxed.Mutable       as UM

import           Utils                             (stream, streamR,
                                                    unsafeShiftRL)
-- | * @appMonoid mempty x = x@
--   * @appMonoid (f <> g) x = appMonoid f (appMonoid g x)@
class (Monoid f) => MonoidAction f a where
    appMonoid :: f -> a -> a

-- | Range update Range query
instance (Monoid m) => MonoidAction m m where
    appMonoid = (<>)
    {-# INLINE appMonoid #-}

-- | Point update Range query
instance MonoidAction () m where
    appMonoid = flip const
    {-# INLINE appMonoid #-}

-- | * @appMonoid mempty x = x@
--   * @appMonoid (f <> g) x = appMonoid f (appMonoid g x)@
--   * @appMonoid f mempty = mempty@
--   * @appMonoid f (x <> y) = appMonoid f x <> appMonoid f y@
data SegTree s a f = SegTree
    (UM.MVector s a)
    (UM.MVector s f)

newSegTree :: (Monoid a, U.Unbox a, Monoid f, U.Unbox f, PrimMonad m)
    => Int -> m (SegTree (PrimState m) a f)
newSegTree n = SegTree
    <$> UM.replicate (2 * extendToPowerOfTwo n) mempty
    <*> UM.replicate (extendToPowerOfTwo n) mempty

buildSegTree :: (Monoid a, U.Unbox a, Monoid f, U.Unbox f, PrimMonad m)
    => U.Vector a -> m (SegTree (PrimState m) a f)
buildSegTree xs = do
    tree <- UM.replicate (2 * n) mempty
    lazy <- UM.replicate n mempty
    U.unsafeCopy (UM.unsafeSlice n (U.length xs) tree) xs
    let seg = SegTree tree lazy
    flip MS.mapM_ (streamR 1 n) $ \i -> do
        updateSegTree seg i
    return seg
  where
    !n = extendToPowerOfTwo $ U.length xs

fullAppAt :: (Monoid a, U.Unbox a, Monoid f, U.Unbox f, MonoidAction f a, PrimMonad m)
    => SegTree (PrimState m) a f -> Int -> f -> m ()
fullAppAt (SegTree tree lazy) k f = do
    tk <- UM.unsafeModify tree (appMonoid f) k
    when (k < UM.length lazy) $ do
        UM.unsafeModify lazy (mappend f) k
{-# INLINE fullAppAt #-}

pushSegTree :: (Monoid a, U.Unbox a, Monoid f, U.Unbox f, MonoidAction f a, PrimMonad m)
    => SegTree (PrimState m) a f -> Int  -> m ()
pushSegTree st@(SegTree tree lazy) k = do
    fk <- UM.unsafeRead lazy k
    fullAppAt st (2 * k) fk
    fullAppAt st (2 * k + 1) fk
    UM.unsafeWrite lazy k mempty
{-# INLINE pushSegTree #-}

updateSegTree :: (Monoid a, U.Unbox a, PrimMonad m)
    => SegTree (PrimState m) a f -> Int -> m ()
updateSegTree (SegTree tree _) k = do
    (<>) <$> UM.unsafeRead tree (2 * k) <*> UM.unsafeRead tree (2 * k + 1)
        >>= UM.unsafeWrite tree k
{-# INLINE updateSegTree #-}

writeSegTree :: (Monoid a, U.Unbox a, Monoid f, U.Unbox f, MonoidAction f a, PrimMonad m)
    => SegTree (PrimState m) a f -> Int -> a -> m ()
writeSegTree st@(SegTree tree lazy) k0 v = do
    let !n = UM.length lazy
        k = k0 + n
        !h = 64 - countLeadingZeros n
    flip MS.mapM_ (streamR 1 h) $ \i -> do
        pushSegTree st (unsafeShiftR k i)
    UM.unsafeWrite tree k v
    flip MS.mapM_ (stream 1 h) $ \i -> do
        updateSegTree st (unsafeShiftR k i)
{-# INLINE writeSegTree #-}

readSegTree :: (Monoid a, U.Unbox a, Monoid f, U.Unbox f, MonoidAction f a, PrimMonad m)
    => SegTree (PrimState m) a f -> Int -> m a
readSegTree st@(SegTree tree lazy) k0 = do
    let !n = UM.length lazy
        k = k0 + n
        !h = 64 - countLeadingZeros n
    flip MS.mapM_ (streamR 1 h) $ \i -> do
        pushSegTree st (unsafeShiftR k i)
    UM.unsafeRead tree k
{-# INLINE readSegTree #-}

mappendFromTo :: (Monoid a, U.Unbox a, Monoid f, U.Unbox f, MonoidAction f a, PrimMonad m)
    => SegTree (PrimState m) a f -> Int -> Int -> m a
mappendFromTo st@(SegTree tree lazy) l0 r0 = do
    let !n = UM.length lazy
        !l = l0 + n
        !r = r0 + n
        !h = 64 - countLeadingZeros n
    flip MS.mapM_ (streamR 1 h) $ \i -> do
        when (unsafeShiftR l i `unsafeShiftL` i /= l) $ do
            pushSegTree st (unsafeShiftR l i)
        when (unsafeShiftR r i `unsafeShiftL` i /= r) $ do
            pushSegTree st (unsafeShiftR r i)
    let calcL l acc
            | l .&. 1 == 1 =
                mappend acc <$> UM.unsafeRead tree l
            | otherwise = return acc

        calcR r acc
            | r .&. 1 == 1 =
                flip mappend acc <$> UM.unsafeRead tree (r - 1)
            | otherwise = return acc

    fix (\loop !accL !accR !l' !r' -> do
        if l' < r'
        then do
            !accL' <- calcL l' accL
            !accR' <- calcR r' accR
            loop accL' accR'
                (unsafeShiftRL (l' + l' .&. 1) 1)
                (unsafeShiftRL (r' - r' .&. 1) 1)
        else return $! accL <> accR
        ) mempty mempty l r
{-# INLINE mappendFromTo #-}

mappendAll :: (Monoid a, U.Unbox a, PrimMonad m)
    => SegTree (PrimState m) a f -> m a
mappendAll (SegTree tree _) = UM.unsafeRead tree 1
{-# INLINE mappendAll #-}

appAt :: (Monoid a, U.Unbox a, Monoid f, U.Unbox f, MonoidAction f a, PrimMonad m)
    => SegTree (PrimState m) a f -> Int -> f -> m ()
appAt st@(SegTree tree lazy) k0 f = do
    let !n = UM.length lazy
        k = k0 + n
        !h = 64 - countLeadingZeros n
    flip MS.mapM_ (streamR 1 h) $ \i -> do
        pushSegTree st (unsafeShiftR k i)
    UM.unsafeModify tree (appMonoid f) k
    flip MS.mapM_ (stream 1 h) $ \i -> do
        updateSegTree st (unsafeShiftR k i)
{-# INLINE appAt #-}

appFromTo :: (Monoid a, U.Unbox a, Monoid f, U.Unbox f, MonoidAction f a, PrimMonad m)
    => SegTree (PrimState m) a f -> Int -> Int -> f -> m ()
appFromTo st@(SegTree tree lazy) l0 r0 f = when (l0 < r0) $ do
    let !n = UM.length lazy
        !l = l0 + n
        !r = r0 + n
        !h = 64 - countLeadingZeros n
    flip MS.mapM_ (streamR 1 h) $ \i -> do
        when (unsafeShiftR l i `unsafeShiftL` i /= l) $ do
            pushSegTree st (unsafeShiftRL l i)
        when (unsafeShiftR r i `unsafeShiftL` i /= r) $ do
            pushSegTree st (unsafeShiftRL (r - 1) i)

    fix (\loop !l' !r' -> when (l' < r') $ do
        when (l' .&. 1 == 1) $ do
            fullAppAt st l' f
        when (r' .&. 1 == 1) $ do
            fullAppAt st (r' - 1) f
        loop (unsafeShiftRL (l' + l' .&. 1) 1) (unsafeShiftRL (r' - r' .&. 1) 1)
        ) l r

    flip MS.mapM_ (stream 1 h) $ \i -> do
        when (unsafeShiftR l i `unsafeShiftL` i /= l) $ do
            updateSegTree st (unsafeShiftRL l i)
        when (unsafeShiftR r i `unsafeShiftL` i /= r) $ do
            updateSegTree st (unsafeShiftRL (r - 1) i)
{-# INLINE appFromTo #-}

extendToPowerOfTwo :: Int -> Int
extendToPowerOfTwo x
    | x > 1 = unsafeShiftRL (-1) (countLeadingZeros (x - 1)) + 1
    | otherwise = 1
