{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}

module Data.UnionFind.Vector where

import           Control.Monad
import           Control.Monad.Primitive
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM

data UnionFind m = UF
    { parent :: UM.MVector m Int
    , rank   :: UM.MVector m Int
    }

nothing :: Int
nothing = -1
{-# INLINE nothing #-}

newUnionFind :: PrimMonad m => Int -> m (UnionFind (PrimState m))
newUnionFind n = UF `liftM` UM.replicate n nothing `ap` UM.replicate n 0
{-# INLINE newUnionFind #-}

findM :: PrimMonad m => UnionFind (PrimState m) -> Int -> m Int
findM uf@UF{..} x = do
    px <- UM.unsafeRead parent x
    if px == nothing
    then return x
    else do
        ppx <- findM uf px
        UM.unsafeWrite parent x ppx
        return ppx
{-# INLINE findM #-}

uniteM :: PrimMonad m => UnionFind (PrimState m) -> Int -> Int -> m ()
uniteM uf@UF{..} x y= do
    px <- findM uf x
    py <- findM uf y
    when (px /= py) $ do
        rx <- UM.unsafeRead rank px
        ry <- UM.unsafeRead rank py
        case compare rx ry of
            LT -> UM.unsafeWrite parent px py
            GT -> UM.unsafeWrite parent py px
            EQ -> do
                UM.unsafeWrite parent px py
                UM.unsafeWrite rank py (ry + 1)
{-# INLINE uniteM #-}

equivM :: PrimMonad m => UnionFind (PrimState m) -> Int -> Int -> m Bool
equivM uf x y = (==) `liftM` findM uf x `ap` findM uf y
{-# INLINE equivM #-}

-- | O(n)
countGroupM :: PrimMonad m => UnionFind (PrimState m) -> m Int
countGroupM UF{..} = U.length . U.filter (==nothing) <$> U.unsafeFreeze parent
{-# INLINE countGroupM #-}
