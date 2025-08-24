module Data.UnionFind.Merge where

import Control.Monad
import Control.Monad.Primitive
import Data.Function
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

data UnionFind mv s a = UF
  { parentOrNegativeSizeUF :: UM.MVector s Int
  , mconcatUF :: mv s a
  }

{- |
>>> import qualified Data.Set as S
>>> import qualified Data.Vector as V
>>> uf <- newUnionFind @V.Vector @(S.Set Int) 3
-}
newUnionFind ::
  (G.Vector v a, Monoid a, PrimMonad m) =>
  Int ->
  m (UnionFind (G.Mutable v) (PrimState m) a)
newUnionFind n = UF <$> UM.replicate n (-1) <*> GM.replicate n mempty
{-# INLINE newUnionFind #-}

{- |
>>> import Data.Bits
>>> import qualified Data.Vector as V
>>> uf <- buildUnionFind (V.generate 3 Xor)
-}
buildUnionFind ::
  (G.Vector v a, PrimMonad m) =>
  v a ->
  m (UnionFind (G.Mutable v) (PrimState m) a)
buildUnionFind v = do
  mv <- G.thaw v
  UF <$> UM.replicate (GM.length mv) (-1) <*> pure mv
{-# INLINE buildUnionFind #-}

findUF ::
  (PrimMonad m) =>
  UnionFind mv (PrimState m) a ->
  Int ->
  m Int
findUF UF{parentOrNegativeSizeUF = uf} x0 = go x0 return
  where
    go !x k = do
      px <- UM.unsafeRead uf x
      if px < 0
        then k x
        else go px $ \ppx -> do
          UM.unsafeWrite uf x ppx
          k ppx
{-# INLINE findUF #-}

sizeUF ::
  (PrimMonad m) =>
  UnionFind mv (PrimState m) a ->
  Int ->
  m Int
sizeUF UF{parentOrNegativeSizeUF = uf} = fix $ \loop x -> do
  px <- UM.unsafeRead uf x
  if px < 0
    then return $! negate px
    else loop px
{-# INLINE sizeUF #-}

{- |
@equivUF uf x y ==> readUF uf x == readUF uf y@

>>> import Data.Monoid
>>> uf <- buildUnionFind @_ @(Sum Int) $ U.replicate 3 1
>>> readUF uf 0
Sum {getSum = 1}
>>> uniteUF_ uf 0 1 >> uniteUF_ uf 1 2
>>> readUF uf 0
Sum {getSum = 3}
-}
readUF ::
  (G.Vector v a, PrimMonad m) =>
  UnionFind (G.Mutable v) (PrimState m) a ->
  Int ->
  m a
readUF uf x = findUF uf x >>= GM.unsafeRead (mconcatUF uf)
{-# INLINE readUF #-}

{- |
>>> import qualified Data.Vector as V
>>> uf <- newUnionFind @V.Vector @String 3
>>> uniteUF_ uf 0 2
>>> writeUF uf 0 "X"
>>> mapM (readUF uf) [0..2]
["X","","X"]

>>> import Data.Monoid
>>> uf <- buildUnionFind @_ @(Sum Int) $ U.replicate 3 1
>>> uniteUF_ uf 0 1 >> uniteUF_ uf 1 2
>>> readUF uf 0
Sum {getSum = 3}
>>> writeUF uf 2 2
>>> readUF uf 0
Sum {getSum = 2}
-}
writeUF ::
  (G.Vector v a, PrimMonad m) =>
  UnionFind (G.Mutable v) (PrimState m) a ->
  Int ->
  a ->
  m ()
writeUF uf i x = findUF uf i >>= flip (GM.unsafeWrite (mconcatUF uf)) x
{-# INLINE writeUF #-}

{- |
>>> import Data.Monoid
>>> uf <- buildUnionFind @_ @(Sum Int) $ U.replicate 3 1
>>> uniteUF_ uf 0 1 >> uniteUF_ uf 1 2
>>> readUF uf 0
Sum {getSum = 3}
>>> modifyUF uf (1-) 2
>>> readUF uf 0
Sum {getSum = -2}
-}
modifyUF ::
  (G.Vector v a, PrimMonad m) =>
  UnionFind (G.Mutable v) (PrimState m) a ->
  (a -> a) ->
  Int ->
  m ()
modifyUF uf f i = findUF uf i >>= GM.unsafeModify (mconcatUF uf) f
{-# INLINE modifyUF #-}

{- |
>>> import Data.Monoid
>>> uf <- buildUnionFind (U.replicate 3 (Sum 1))
>>> uniteUF uf 0 2
True
>>> uniteUF uf 0 2
False
>>> mapM (readUF uf) [0..2]
[Sum {getSum = 2.0},Sum {getSum = 1.0},Sum {getSum = 2.0}]
-}
uniteUF ::
  (G.Vector v a, Monoid a, PrimMonad m) =>
  UnionFind (G.Mutable v) (PrimState m) a ->
  Int ->
  Int ->
  m Bool
uniteUF uf x y = do
  px <- findUF uf x
  py <- findUF uf y
  if px == py
    then return False
    else do
      rx <- UM.unsafeRead (parentOrNegativeSizeUF uf) px
      ry <- UM.unsafeRead (parentOrNegativeSizeUF uf) py
      if rx < ry
        then do
          UM.unsafeModify (parentOrNegativeSizeUF uf) (+ ry) px
          UM.unsafeWrite (parentOrNegativeSizeUF uf) py px
          mx <- GM.unsafeRead (mconcatUF uf) px
          my <- GM.unsafeRead (mconcatUF uf) py
          GM.unsafeWrite (mconcatUF uf) px $! mx <> my
          GM.unsafeWrite (mconcatUF uf) py $! mempty
        else do
          UM.unsafeModify (parentOrNegativeSizeUF uf) (+ rx) py
          UM.unsafeWrite (parentOrNegativeSizeUF uf) px py
          mx <- GM.unsafeRead (mconcatUF uf) px
          my <- GM.unsafeRead (mconcatUF uf) py
          GM.unsafeWrite (mconcatUF uf) py $! mx <> my
          GM.unsafeWrite (mconcatUF uf) px $! mempty
      return True
{-# INLINE uniteUF #-}

{- |
>>> import qualified Data.Set as S
>>> import qualified Data.Vector as V
>>> uf <- buildUnionFind (V.generate 3 S.singleton)
>>> uniteUF_ uf 0 2
>>> mapM (readUF uf) [0..2]
[fromList [0,2],fromList [1],fromList [0,2]]


>>> import qualified Data.Vector as V
>>> uf <- buildUnionFind (V.generate 3 (:[]))
>>> uniteUF_ uf 0 1
>>> uniteUF_ uf 2 0
>>> mapM (readUF uf) [0..2]
[[2,0,1],[2,0,1],[2,0,1]]
-}
uniteUF_ ::
  (G.Vector v a, Monoid a, PrimMonad m) =>
  UnionFind (G.Mutable v) (PrimState m) a ->
  Int ->
  Int ->
  m ()
uniteUF_ uf x y = void $ uniteUF uf x y
{-# INLINE uniteUF_ #-}

equivUF ::
  (PrimMonad m) =>
  UnionFind mv (PrimState m) a ->
  Int ->
  Int ->
  m Bool
equivUF uf x y = (==) `liftM` findUF uf x `ap` findUF uf y
{-# INLINE equivUF #-}

-- | /O(n)/
countGroupUF ::
  (PrimMonad m) =>
  UnionFind mv (PrimState m) a ->
  m Int
countGroupUF uf = U.length . U.filter (< 0) <$> U.freeze (parentOrNegativeSizeUF uf)
{-# INLINE countGroupUF #-}
