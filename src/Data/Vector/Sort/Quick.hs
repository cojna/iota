module Data.Vector.Sort.Quick where

import Control.Monad
import Control.Monad.Primitive
import Data.Bits
import Data.Function
import qualified Data.Vector.Generic.Mutable as GM

import System.Random.XoRoShiRo

{- | Random Pivot Quick Sort

 /O(n log n)/
-}
quickSort ::
  (Ord a, PrimMonad m, GM.MVector mv a) =>
  mv (PrimState m) a ->
  m ()
quickSort = quickSortBy compare
{-# INLINE quickSort #-}

{- | Random Pivot Quick Sort

 /O(n log n)/
-}
quickSortBy ::
  (PrimMonad m, GM.MVector mv a) =>
  (a -> a -> Ordering) ->
  mv (PrimState m) a ->
  m ()
quickSortBy cmp mv0 = do
  rng <- newRNGWithSeed $ fromIntegral (GM.length mv0)
  fix
    ( \loop !mvec ->
        if GM.length mvec > 32
          then do
            i <- (`mod` GM.length mvec) <$> nextInt rng
            pivot <- GM.read mvec i
            cut <- pivotPartitionBy cmp mvec pivot
            loop (GM.take cut mvec)
            loop (GM.drop cut mvec)
          else insertionSortBy cmp mvec
    )
    mv0
{-# INLINE quickSortBy #-}

{- | Random Pivot Quick Select

 /O(n)/
-}
quickSelect ::
  (Ord a, PrimMonad m, GM.MVector mv a) =>
  mv (PrimState m) a ->
  Int ->
  m a
quickSelect = quickSelectBy compare
{-# INLINE quickSelect #-}

{- | Random Pivot Quick Select

 /O(n)/
-}
quickSelectBy ::
  (PrimMonad m, GM.MVector mv a) =>
  (a -> a -> Ordering) ->
  mv (PrimState m) a ->
  Int ->
  m a
quickSelectBy cmp mv0 k0 = do
  rng <-
    newRNGWithSeed
      -- FNV-1a
      . foldl (\h x -> xor h x * 0x100000001b3) 0xcbf29ce484222325
      $ map fromIntegral [k0, GM.length mv0]
  fix
    ( \loop !mvec !k -> do
        if GM.length mvec > 32
          then do
            i <- (`mod` GM.length mvec) <$> nextInt rng
            pivot <- GM.read mvec i
            cut <- pivotPartitionBy cmp mvec pivot
            if k < cut
              then loop (GM.take cut mvec) k
              else loop (GM.drop cut mvec) (k - cut)
          else do
            insertionSortBy cmp mvec
            GM.unsafeRead mvec k
    )
    mv0
    k0
{-# INLINE quickSelectBy #-}

pivotPartitionBy ::
  (PrimMonad m, GM.MVector mv a) =>
  (a -> a -> Ordering) ->
  mv (PrimState m) a ->
  a ->
  m Int
pivotPartitionBy cmp vec !pivot =
  fix
    ( \loop !l !r -> do
        !l' <-
          fix
            ( \loopL !i -> do
                vi <- GM.unsafeRead vec i
                case cmp vi pivot of
                  LT -> loopL (i + 1)
                  _ -> return i
            )
            l
        !r' <-
          fix
            ( \loopR !i -> do
                vi <- GM.unsafeRead vec i
                case cmp pivot vi of
                  LT -> loopR (i - 1)
                  _ -> return i
            )
            (r - 1)
        if l' < r'
          then do
            GM.unsafeSwap vec l' r'
            loop (l' + 1) r'
          else return l'
    )
    0
    (GM.length vec)
{-# INLINE pivotPartitionBy #-}

getMedian3PivotBy ::
  (PrimMonad m, GM.MVector mv a) =>
  (a -> a -> Ordering) ->
  mv (PrimState m) a ->
  m a
getMedian3PivotBy cmp vec =
  medianBy cmp
    <$> GM.unsafeRead vec 0
    <*> GM.unsafeRead vec (unsafeShiftR (GM.length vec) 1)
    <*> GM.unsafeRead vec (GM.length vec - 1)
{-# INLINE getMedian3PivotBy #-}

{- |
>>> medianBy compare 3 1 2
2
-}
medianBy :: (a -> a -> Ordering) -> a -> a -> a -> a
medianBy cmp x y z = case cmp x y of
  LT -> case cmp y z of
    LT -> y
    _ -> case cmp x z of
      LT -> z
      _ -> x
  _ -> case cmp x z of
    LT -> x
    _ -> case cmp y z of
      LT -> z
      _ -> y
{-# INLINE medianBy #-}

insertionSortBy ::
  (GM.MVector mv a, PrimMonad m) =>
  (a -> a -> Ordering) ->
  mv (PrimState m) a ->
  m ()
insertionSortBy cmp mvec = do
  fix
    ( \outer !i -> when (i < n) $ do
        v0 <- GM.unsafeRead mvec 0
        vi <- GM.unsafeRead mvec i
        case cmp v0 vi of
          GT -> do
            fix
              ( \inner !j -> when (j > 0) $ do
                  GM.unsafeRead mvec (j - 1) >>= GM.unsafeWrite mvec j
                  inner (j - 1)
              )
              i
            GM.unsafeWrite mvec 0 vi
            outer (i + 1)
          _ -> do
            fix
              ( \inner !j -> do
                  vj' <- GM.unsafeRead mvec (j - 1)
                  case cmp vj' vi of
                    GT -> do
                      GM.unsafeWrite mvec j vj'
                      inner (j - 1)
                    _ -> do
                      GM.unsafeWrite mvec j vi
                      outer (i + 1)
              )
              i
    )
    1
  where
    !n = GM.length mvec
{-# INLINE insertionSortBy #-}
