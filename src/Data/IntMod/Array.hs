{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UnboxedTuples         #-}
module Data.IntMod.Array where

import           Control.Monad
import           Control.Monad.ST
import           Data.Array.Base
import           Data.Array.IO.Internals (IOUArray (..))
import           Data.Coerce
import           Data.IntMod
import           GHC.Arr
import           GHC.Exts
import           GHC.ST                  (ST (..))

instance IArray UArray IntMod where
    {-# INLINE bounds #-}
    bounds (UArray l u _ _) = (l,u)
    {-# INLINE numElements #-}
    numElements (UArray _ _ n _) = n
    {-# INLINE unsafeArray #-}
    unsafeArray lu ies = case runST (unsafeArrayUArray lu (coerce ies) (0 :: Int)) of
        (UArray l u n arr) -> UArray l u n arr
    {-# INLINE unsafeAt #-}
    unsafeAt (UArray _ _ _ arr#) (I# i#) = coerce $ I# (indexIntArray# arr# i#)
    {-# INLINE unsafeReplace #-}
    unsafeReplace (UArray l u n arr) ies = case runST (unsafeReplaceUArray (UArray l u n arr) (coerce ies :: [(Int, Int)])) of
        (UArray l u n arr) -> UArray l u n arr
    {-# INLINE unsafeAccum #-}
    unsafeAccum f (UArray l u n arr) ies =  case runST (unsafeAccumUArray ((getIntMod.).f.coerce) (UArray l u n arr) $ coerce ies) of
         (UArray l u n arr) -> UArray l u n arr
    {-# INLINE unsafeAccumArray #-}
    unsafeAccumArray f initialValue lu ies = case runST (unsafeAccumArrayUArray ((getIntMod.).f.coerce) (coerce initialValue) lu $ coerce ies) of
        (UArray l u n arr) -> UArray l u n arr

instance MArray (STUArray s) IntMod (ST s) where
    {-# INLINE getBounds #-}
    getBounds (STUArray l u _ _) = return (l,u)
    {-# INLINE getNumElements #-}
    getNumElements (STUArray _ _ n _) = return n
    {-# INLINE unsafeNewArray_ #-}
    unsafeNewArray_ (l,u) = do
        STUArray l u n marr <- unsafeNewArraySTUArray_ (l,u) wORD_SCALE
        return $ STUArray l u n marr
    {-# INLINE newArray_ #-}
    newArray_ arrBounds = do
        STUArray l u n marr <- newArray arrBounds (0 :: Int)
        return $ STUArray l u n marr
    {-# INLINE unsafeRead #-}
    unsafeRead (STUArray _ _ _ marr#) (I# i#) = fmap coerce $ ST $ \s1# ->
        case readIntArray# marr# i# s1# of { (# s2#, e# #) ->
        (# s2#, I# e# #) }
    {-# INLINE unsafeWrite #-}
    unsafeWrite (STUArray _ _ _ marr#) (I# i#) (IntMod (I# e#)) = ST $ \s1# ->
        case writeIntArray# marr# i# e# s1# of { s2# ->
        (# s2#, () #) }

instance MArray IOUArray IntMod IO where
    {-# INLINE getBounds #-}
    getBounds (IOUArray arr) = stToIO $ getBounds arr
    {-# INLINE getNumElements #-}
    getNumElements (IOUArray arr) = stToIO $ getNumElements arr
    {-# INLINE newArray #-}
    newArray lu initialValue = stToIO $ do
        marr <- newArray lu initialValue; return (IOUArray marr)
    {-# INLINE unsafeNewArray_ #-}
    unsafeNewArray_ lu = stToIO $ do
        marr <- unsafeNewArray_ lu; return (IOUArray marr)
    {-# INLINE newArray_ #-}
    newArray_ = unsafeNewArray_
    {-# INLINE unsafeRead #-}
    unsafeRead (IOUArray marr) i = stToIO (unsafeRead marr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite (IOUArray marr) i e = stToIO (unsafeWrite marr i e)
