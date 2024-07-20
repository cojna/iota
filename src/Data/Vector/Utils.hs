module Data.Vector.Utils where

import Control.Monad
import Control.Monad.Primitive
import Data.Function
import qualified Data.Vector.Fusion.Stream.Monadic as MS
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U

import My.Prelude

chunks :: (G.Vector u a, G.Vector v (u a)) => Int -> u a -> v (u a)
chunks n = G.unfoldr $ \u ->
  if G.null u then Nothing else Just $ G.splitAt n u

group :: (Eq a, G.Vector u a, G.Vector v (u a)) => u a -> v (u a)
group = groupBy (==)

groupBy :: (G.Vector u a, G.Vector v (u a)) => (a -> a -> Bool) -> u a -> v (u a)
groupBy eq = G.unfoldr $ \u ->
  if G.null u
    then Nothing
    else case G.unsafeHead u of
      u0 -> Just $ G.span (eq u0) u

tuples2 :: (G.Vector v a, G.Vector v (a, a)) => v a -> v (a, a)
tuples2 = G.unfoldr $ \v ->
  if G.length v >= 2
    then
      ( let !x = G.unsafeIndex v 0
            !y = G.unsafeIndex v 1
         in Just ((x, y), G.drop 2 v)
      )
    else Nothing

tuples2N :: (G.Vector v a, G.Vector v (a, a)) => Int -> v a -> v (a, a)
tuples2N n = G.unfoldrExactN n $ \v ->
  let !x = G.unsafeIndex v 0
      !y = G.unsafeIndex v 1
   in ((x, y), G.drop 2 v)

tuples3 :: (G.Vector v a, G.Vector v (a, a, a)) => v a -> v (a, a, a)
tuples3 = G.unfoldr $ \v ->
  if G.length v >= 3
    then
      ( let !x = G.unsafeIndex v 0
            !y = G.unsafeIndex v 1
            !z = G.unsafeIndex v 2
         in Just ((x, y, z), G.drop 3 v)
      )
    else Nothing

tuples3N :: (G.Vector v a, G.Vector v (a, a, a)) => Int -> v a -> v (a, a, a)
tuples3N n = G.unfoldrExactN n $ \v ->
  let !x = G.unsafeIndex v 0
      !y = G.unsafeIndex v 1
      !z = G.unsafeIndex v 2
   in ((x, y, z), G.drop 3 v)

tuples4 :: (G.Vector v a, G.Vector v (a, a, a, a)) => v a -> v (a, a, a, a)
tuples4 = G.unfoldr $ \v ->
  if G.length v >= 3
    then
      ( let !x = G.unsafeIndex v 0
            !y = G.unsafeIndex v 1
            !z = G.unsafeIndex v 2
            !w = G.unsafeIndex v 3
         in Just ((x, y, z, w), G.drop 4 v)
      )
    else Nothing

tuples4N ::
  (G.Vector v a, G.Vector v (a, a, a, a)) =>
  Int ->
  v a ->
  v (a, a, a, a)
tuples4N n = G.unfoldrExactN n $ \v ->
  let !x = G.unsafeIndex v 0
      !y = G.unsafeIndex v 1
      !z = G.unsafeIndex v 2
      !w = G.unsafeIndex v 3
   in ((x, y, z, w), G.drop 4 v)

{- |
>>> import qualified Data.Vector.Unboxed as U
>>> transpose 2 3 (U.fromList "abcdef")
"adbecf"
-}
transpose :: (G.Vector v a) => Int -> Int -> v a -> v a
transpose h w vec = G.create $ do
  buf <- GM.unsafeNew (w * h)
  rep h $ \x -> do
    G.foldM'_
      ( \pos mxy -> do
          GM.write buf pos mxy
          pure $! pos + h
      )
      x
      (G.slice (x * w) w vec)
  return buf

{- |
>>> U.modify (void . nextPermutation) $ U.fromList "abc"
"acb"
>>> U.modify (void . nextPermutation) $ U.fromList "cba"
"cba"
>>> U.modify (void . nextPermutation) $ U.fromList "a"
"a"
>>> U.modify (void . nextPermutation) $ U.fromList ""
""
-}
nextPermutation :: (GM.MVector mv a, Ord a, PrimMonad m) => mv (PrimState m) a -> m Bool
nextPermutation v = do
  fix
    ( \loop !i !k !l ->
        if i < n
          then do
            prev <- GM.unsafeRead v (i - 1)
            cur <- GM.unsafeRead v i
            if prev < cur
              then loop (i + 1) (i - 1) i
              else
                if k /= nothing
                  then do
                    vk <- GM.unsafeRead v k
                    if vk < cur
                      then loop (i + 1) k i
                      else loop (i + 1) k l
                  else loop (i + 1) k l
          else do
            if k /= nothing
              then do
                GM.unsafeSwap v k l
                GM.reverse $ GM.unsafeDrop (k + 1) v
                return True
              else return False
    )
    1
    nothing
    nothing
  where
    n = GM.length v
    nothing = -1
{-# INLINE nextPermutation #-}

{- |
>>> U.modify (void . prevPermutation) $ U.fromList "acb"
"abc"
>>> U.modify (void . prevPermutation) $ U.fromList "abc"
"abc"
>>> U.modify (void . prevPermutation) $ U.fromList "a"
"a"
>>> U.modify (void . prevPermutation) $ U.fromList ""
""
-}
prevPermutation :: (GM.MVector mv a, Ord a, PrimMonad m) => mv (PrimState m) a -> m Bool
prevPermutation v = do
  fix
    ( \loop !i !k !l ->
        if i < n
          then do
            prev <- GM.unsafeRead v (i - 1)
            cur <- GM.unsafeRead v i
            if prev > cur
              then loop (i + 1) (i - 1) i
              else
                if k /= nothing
                  then do
                    vk <- GM.unsafeRead v k
                    if vk > cur
                      then loop (i + 1) k i
                      else loop (i + 1) k l
                  else loop (i + 1) k l
          else do
            if k /= nothing
              then do
                GM.unsafeSwap v k l
                GM.reverse $ GM.unsafeDrop (k + 1) v
                return True
              else return False
    )
    1
    nothing
    nothing
  where
    n = GM.length v
    nothing = -1
{-# INLINE prevPermutation #-}

{- |
>>> U.modify (void . flip kthPermutation (120 - 1)) $ U.fromList "abcde"
"edcba"
>>> U.modify (void . flip kthPermutation 0) $ U.fromList "abcde"
"abcde"
>>> U.modify (void . flip kthPermutation 1) $ U.fromList "abcde"
"abced"
>>> U.modify (void . flip kthPermutation 999) $ U.fromList "abcde"
"abcde"
-}
kthPermutation :: (GM.MVector mv a, Ord a, PrimMonad m) => mv (PrimState m) a -> Int -> m Bool
kthPermutation v k0 = do
  if GM.length v > cacheSize || fact U.! GM.length v <= k0
    then return False
    else do
      void $
        MS.foldlM'
          ( \k i -> do
              let f = U.unsafeIndex fact (n - i - 1)
                  (q, r) = quotRem k f
              flip MS.mapM_ (i + q + 1 >.. i + 1) $ \j -> do
                GM.unsafeSwap v j (j - 1)
              return r
          )
          k0
          (0 ..< n)
      return True
  where
    n = GM.length v
    cacheSize = 21
    !fact = U.scanl' (*) 1 (U.generate (cacheSize - 1) (+ 1))
{-# INLINE kthPermutation #-}
