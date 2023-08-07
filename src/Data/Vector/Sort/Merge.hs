{-# LANGUAGE TypeFamilies #-}

module Data.Vector.Sort.Merge where

import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Bits
import Data.Function
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM

{- |
>>> import qualified Data.Vector.Unboxed as U
>>> inversionNumber $ U.fromList "312"
2
>>> inversionNumber $ U.fromList "100"
2
>>> inversionNumber $ U.fromList "123"
0
-}
inversionNumber :: (G.Vector v a, Ord a) => v a -> Int
inversionNumber xs = runST $ do
  mvec <- G.thaw xs
  mergeSort mvec
{-# INLINE inversionNumber #-}

{- |
>>> import qualified Data.Vector.Unboxed as U
>>> U.modify mergeSort_ $ U.fromList "3610425"
"0123456"
>>> import Data.List (sort)
prop> \(xs::[Int]) -> U.fromList (sort xs) == U.modify mergeSort_ (U.fromList xs)
+++ OK, passed 100 tests.
-}
mergeSort ::
  (GM.MVector mv a, Ord a, PrimMonad m) =>
  mv (PrimState m) a ->
  -- | inversion number
  m Int
mergeSort = mergeSortBy compare
{-# INLINE mergeSort #-}

mergeSortBy ::
  (GM.MVector mv a, PrimMonad m) =>
  (a -> a -> Ordering) ->
  mv (PrimState m) a ->
  -- | inversion number
  m Int
mergeSortBy cmp mvec0 = do
  buf <- GM.unsafeNew (quot (GM.length mvec0 + 1) 2)
  go buf mvec0
  where
    go !buf !mvec
      | n <= 16 = insertionSortBy cmp mvec
      | otherwise = do
          let !numL = unsafeShiftR (n + 1) 1
              !numR = n - numL
              (!buf', !vecR) = GM.splitAt numL mvec
              !vecL = GM.unsafeTake numL buf
          GM.unsafeCopy vecL buf'
          !invL <- go buf' vecL
          !invR <- go buf' vecR
          fix
            ( \loop !invNum !posL !posR ->
                if posL < numL && posR < numR
                  then do
                    vl <- GM.unsafeRead vecL posL
                    vr <- GM.unsafeRead vecR posR
                    case cmp vl vr of
                      GT -> do
                        GM.unsafeWrite mvec (posL + posR) vr
                        loop (invNum + numL - posL) posL (posR + 1)
                      _ -> do
                        GM.unsafeWrite mvec (posL + posR) vl
                        loop invNum (posL + 1) posR
                  else do
                    when (posL < numL) $ do
                      GM.unsafeCopy
                        (GM.unsafeSlice (posL + posR) (numL - posL) mvec)
                        (GM.unsafeSlice posL (numL - posL) vecL)
                    return invNum
            )
            (invL + invR)
            0
            0
      where
        !n = GM.length mvec
{-# INLINE mergeSortBy #-}

mergeSort_ :: (GM.MVector mv a, Ord a, PrimMonad m) => mv (PrimState m) a -> m ()
mergeSort_ = void . mergeSort
{-# INLINE mergeSort_ #-}

mergeSortBy_ ::
  (GM.MVector mv a, PrimMonad m) =>
  (a -> a -> Ordering) ->
  mv (PrimState m) a ->
  m ()
mergeSortBy_ cmp = void . mergeSortBy cmp
{-# INLINE mergeSortBy_ #-}

{- |
>>> import qualified Data.Vector.Unboxed as U
>>> U.modify insertionSort_ $ U.fromList "3610425"
"0123456"
>>> U.modify insertionSort_ $ U.fromList ""
""
>>> U.modify insertionSort_ $ U.fromList "x"
"x"
>>> U.modify insertionSort_ $ U.fromList "ba"
"ab"
>>> import Data.List (sort)
prop> \(xs::[Int]) -> U.fromList (sort xs) == U.modify insertionSort_ (U.fromList xs)
+++ OK, passed 100 tests.
-}
insertionSort ::
  (GM.MVector mv a, Ord a, PrimMonad m) =>
  mv (PrimState m) a ->
  -- | inversion number
  m Int
insertionSort = insertionSortBy compare
{-# INLINE insertionSort #-}

insertionSortBy ::
  (GM.MVector mv a, PrimMonad m) =>
  (a -> a -> Ordering) ->
  mv (PrimState m) a ->
  -- | inversion number
  m Int
insertionSortBy cmp mvec = do
  fix
    ( \outer !invNum !i ->
        if i < n
          then do
            v0 <- GM.unsafeRead mvec 0
            vi <- GM.unsafeRead mvec i
            case cmp v0 vi of
              GT -> do
                fix
                  ( \inner j -> when (j > 0) $ do
                      GM.unsafeRead mvec (j - 1) >>= GM.unsafeWrite mvec j
                      inner (j - 1)
                  )
                  i
                GM.unsafeWrite mvec 0 vi
                outer (invNum + i) (i + 1)
              _ -> do
                fix
                  ( \inner j -> do
                      vj' <- GM.unsafeRead mvec (j - 1)
                      case cmp vj' vi of
                        GT -> do
                          GM.unsafeWrite mvec j vj'
                          inner (j - 1)
                        _ -> do
                          GM.unsafeWrite mvec j vi
                          outer (invNum + i - j) (i + 1)
                  )
                  i
          else return invNum
    )
    0
    1
  where
    !n = GM.length mvec
{-# INLINE insertionSortBy #-}

insertionSort_ :: (GM.MVector mv a, Ord a, PrimMonad m) => mv (PrimState m) a -> m ()
insertionSort_ = void . insertionSort
{-# INLINE insertionSort_ #-}

insertionSortBy_ ::
  (GM.MVector mv a, PrimMonad m) =>
  (a -> a -> Ordering) ->
  mv (PrimState m) a ->
  m ()
insertionSortBy_ cmp = void . insertionSortBy cmp
{-# INLINE insertionSortBy_ #-}
