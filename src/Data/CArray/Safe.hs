{-# LANGUAGE BangPatterns, Safe #-}

module Data.CArray.Safe where

import Data.Function
import           Foreign
import Utils.Safe

data CArray a = CA
    { lengthCA :: !Int
    , getPtrCA :: !(ForeignPtr a)
    }

newCArray :: (Storable a) => Int -> IO (CArray a)
newCArray n = CA n <$> mallocForeignPtrArray n
{-# INLINE newCArray #-}

replicateCA :: (Storable a) => Int -> a -> IO (CArray a)
replicateCA n x = do
    fp <- mallocForeignPtrArray n
    withForeignPtr fp $ \p ->
        rep n $ \i ->
            pokeElemOff p i x
    return $ CA n fp
{-# INLINE replicateCA #-}

unfoldrNCA :: (Storable a) => Int -> (b -> Maybe (a, b)) -> b -> IO (CArray a)
unfoldrNCA n f x0 = do
    fp <- mallocForeignPtrArray n
    withForeignPtr fp $ \p ->
        fix `flip` 0 `flip` x0 $ \loop !i !x ->
            case f x of
                Just (y, x') -> do
                    pokeElemOff p i y
                    loop (i + 1) x'
                Nothing -> return $ CA n fp
{-# INLINE unfoldrNCA #-}

unsafeReadCA :: (Storable a) => CArray a -> Int -> IO a
unsafeReadCA (CA n fp) i = withForeignPtr fp $ \p ->
    peekElemOff p i
{-# INLINE unsafeReadCA #-}

unsafeWriteCA :: (Storable a) => CArray a -> Int -> a -> IO ()
unsafeWriteCA (CA n fp) i x = withForeignPtr fp $ \p ->
    pokeElemOff p i x
{-# INLINE unsafeWriteCA #-}

unsafeModifyCA :: (Storable a) => CArray a -> (a -> a) -> Int -> IO ()
unsafeModifyCA (CA n fp) f i = withForeignPtr fp $ \p ->
    peekElemOff p i >>= pokeElemOff p i . f
{-# INLINE unsafeModifyCA #-}

unsafeSwapCA :: (Storable a) => CArray a -> Int -> Int -> IO ()
unsafeSwapCA (CA n fp) i j = withForeignPtr fp $ \p -> do
    tmp <- peekElemOff p i
    peekElemOff p j >>= pokeElemOff p i
    pokeElemOff p j tmp
{-# INLINE unsafeSwapCA #-}

forM_CA :: (Storable a) => CArray a -> (a -> IO ()) -> IO ()
forM_CA (CA n fp) f = withForeignPtr fp $ \p -> do
    rep n $ \i -> do
        peekElemOff p i >>= f
{-# INLINE forM_CA #-}

iforM_CA :: (Storable a) => CArray a -> (Int -> a -> IO ()) -> IO ()
iforM_CA (CA n fp) f = withForeignPtr fp $ \p -> do
    rep n $ \i -> do
        peekElemOff p i >>= f i
{-# INLINE iforM_CA #-}
