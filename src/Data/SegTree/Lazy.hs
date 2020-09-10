{-# LANGUAGE BangPatterns, MultiParamTypeClasses, TypeApplications #-}

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

data SegTreeLazy s a f = SegTreeLazy
    (UM.MVector s a)
    (UM.MVector s f)

newSegTreeLazy :: (Monoid a, U.Unbox a, Monoid f, U.Unbox f, PrimMonad m)
    => Int -> m (SegTreeLazy (PrimState m) a f)
newSegTreeLazy n = SegTreeLazy
    <$> UM.replicate (2 * extendToPowerOfTwo n) mempty
    <*> UM.replicate (extendToPowerOfTwo n) mempty

buildSegTreeLazy :: (Monoid a, U.Unbox a, Monoid f, U.Unbox f, PrimMonad m)
    => U.Vector a -> m (SegTreeLazy (PrimState m) a f)
buildSegTreeLazy xs = do
    tree <- UM.replicate (2 * n) mempty
    lazy <- UM.replicate n mempty
    U.unsafeCopy (UM.unsafeSlice n (U.length xs) tree) xs
    let seg = SegTreeLazy tree lazy
    flip MS.mapM_ (streamR 1 n) $ \i -> do
        updateSegTreeLazy seg i
    return seg
  where
    !n = extendToPowerOfTwo $ U.length xs

class (Monoid f) => MonoidAction f a where
    appMonoid :: f -> a -> a

appAllAt :: (Monoid a, U.Unbox a, Monoid f, U.Unbox f, MonoidAction f a, PrimMonad m)
    => SegTreeLazy (PrimState m) a f -> Int -> f -> m ()
appAllAt (SegTreeLazy tree lazy) k f = do
    tk <- UM.unsafeModify tree (appMonoid f) k
    when (k < UM.length lazy) $ do
        UM.unsafeModify lazy (mappend f) k
{-# INLINE appAllAt #-}

pushSegTreeLazy :: (Monoid a, U.Unbox a, Monoid f, U.Unbox f, MonoidAction f a, PrimMonad m)
    => SegTreeLazy (PrimState m) a f -> Int  -> m ()
pushSegTreeLazy st@(SegTreeLazy tree lazy) k = do
    fk <- UM.unsafeRead lazy k
    appAllAt st (2 * k) fk
    appAllAt st (2 * k + 1) fk
    UM.unsafeWrite lazy k mempty
{-# INLINE pushSegTreeLazy #-}

updateSegTreeLazy :: (Monoid a, U.Unbox a, PrimMonad m)
    => SegTreeLazy (PrimState m) a f -> Int -> m ()
updateSegTreeLazy (SegTreeLazy tree _) k = do
    (<>) <$> UM.unsafeRead tree (2 * k) <*> UM.unsafeRead tree (2 * k + 1)
        >>= UM.unsafeWrite tree k
{-# INLINE updateSegTreeLazy #-}

writeSegTreeLazy :: (Monoid a, U.Unbox a, Monoid f, U.Unbox f, MonoidAction f a, PrimMonad m)
    => SegTreeLazy (PrimState m) a f -> Int -> a -> m ()
writeSegTreeLazy st@(SegTreeLazy tree lazy) k0 v = do
    let !n = UM.length lazy
        k = k0 + n
        !h = 64 - countLeadingZeros n
    flip MS.mapM_ (streamR 1 h) $ \i -> do
        pushSegTreeLazy st (unsafeShiftR k i)
    UM.unsafeWrite tree k v
    flip MS.mapM_ (stream 1 h) $ \i -> do
        updateSegTreeLazy st (unsafeShiftR k i)
{-# INLINE writeSegTreeLazy #-}

readSegTreeLazy :: (Monoid a, U.Unbox a, Monoid f, U.Unbox f, MonoidAction f a, PrimMonad m)
    => SegTreeLazy (PrimState m) a f -> Int -> m a
readSegTreeLazy st@(SegTreeLazy tree lazy) k0 = do
    let !n = UM.length lazy
        k = k0 + n
        !h = 64 - countLeadingZeros n
    flip MS.mapM_ (streamR 1 h) $ \i -> do
        pushSegTreeLazy st (unsafeShiftR k i)
    UM.unsafeRead tree k
{-# INLINE readSegTreeLazy #-}

mappendFromTo :: (Monoid a, U.Unbox a, Monoid f, U.Unbox f, MonoidAction f a, PrimMonad m)
    => SegTreeLazy (PrimState m) a f -> Int -> Int -> m a
mappendFromTo st@(SegTreeLazy tree lazy) l0 r0 = do
    let !n = UM.length lazy
        !l = l0 + n
        !r = r0 + n
        !h = 64 - countLeadingZeros n
    flip MS.mapM_ (streamR 1 h) $ \i -> do
        when (unsafeShiftR l i `unsafeShiftL` i /= l) $ do
            pushSegTreeLazy st (unsafeShiftR l i)
        when (unsafeShiftR r i `unsafeShiftL` i /= r) $ do
            pushSegTreeLazy st (unsafeShiftR r i)
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
    => SegTreeLazy (PrimState m) a f -> m a
mappendAll (SegTreeLazy tree _) = UM.unsafeRead tree 1
{-# INLINE mappendAll #-}

appAt :: (Monoid a, U.Unbox a, Monoid f, U.Unbox f, MonoidAction f a, PrimMonad m)
    => SegTreeLazy (PrimState m) a f -> Int -> f -> m ()
appAt st@(SegTreeLazy tree lazy) k0 f = do
    let !n = UM.length lazy
        k = k0 + n
        !h = 64 - countLeadingZeros n
    flip MS.mapM_ (streamR 1 h) $ \i -> do
        pushSegTreeLazy st (unsafeShiftR k i)
    UM.unsafeModify tree (appMonoid f) k
    flip MS.mapM_ (stream 1 h) $ \i -> do
        updateSegTreeLazy st (unsafeShiftR k i)
{-# INLINE appAt #-}

appFromTo :: (Monoid a, U.Unbox a, Monoid f, U.Unbox f, MonoidAction f a, PrimMonad m)
    => SegTreeLazy (PrimState m) a f -> Int -> Int -> f -> m ()
appFromTo st@(SegTreeLazy tree lazy) l0 r0 f = when (l0 < r0) $ do
    let !n = UM.length lazy
        !l = l0 + n
        !r = r0 + n
        !h = 64 - countLeadingZeros n
    flip MS.mapM_ (streamR 1 h) $ \i -> do
        when (unsafeShiftR l i `unsafeShiftL` i /= l) $ do
            pushSegTreeLazy st (unsafeShiftRL l i)
        when (unsafeShiftR r i `unsafeShiftL` i /= r) $ do
            pushSegTreeLazy st (unsafeShiftRL (r - 1) i)

    fix (\loop !l' !r' -> when (l' < r') $ do
        when (l' .&. 1 == 1) $ do
            appAllAt st l' f
        when (r' .&. 1 == 1) $ do
            appAllAt st (r' - 1) f
        loop (unsafeShiftRL (l' + l' .&. 1) 1) (unsafeShiftRL (r' - r' .&. 1) 1)
        ) l r

    flip MS.mapM_ (stream 1 h) $ \i -> do
        when (unsafeShiftR l i `unsafeShiftL` i /= l) $ do
            updateSegTreeLazy st (unsafeShiftRL l i)
        when (unsafeShiftR r i `unsafeShiftL` i /= r) $ do
            updateSegTreeLazy st (unsafeShiftRL (r - 1) i)
{-# INLINE appFromTo #-}

extendToPowerOfTwo :: Int -> Int
extendToPowerOfTwo x
    | x > 1 = unsafeShiftRL (-1) (countLeadingZeros (x - 1)) + 1
    | otherwise = 1
