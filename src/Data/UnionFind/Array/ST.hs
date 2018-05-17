{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}

module Data.UnionFind.Array.ST where

import           Control.Monad
import           Control.Monad.ST
import           Data.Array.Base
import           Data.Array.ST
import           Data.Function

nothing :: Int
nothing = -1
{-# INLINE nothing #-}

type Parent = Int
type Rank = Int
data UnionFindST s = UF
    { size   :: !Int
    , parent :: !(STUArray s Int Parent)
    , rank   :: !(STUArray s Int Rank)
    }

newUnionFindST :: Int -> ST s (UnionFindST s)
newUnionFindST n = UF n
    <$> newArray (0, n - 1) nothing
    <*> newArray (0, n - 1) 0

findM :: UnionFindST s -> Int -> ST s Parent
findM uf@UF{..} x = do
    px <- unsafeRead parent x
    if px == nothing
    then return x
    else do
        ppx <- findM uf px
        unsafeWrite parent x ppx
        return ppx

uniteM :: UnionFindST s -> Int -> Int -> ST s ()
uniteM uf@UF{..} x y = do
    px <- findM uf x
    py <- findM uf y
    when (px /= py) $ do
        rx <- unsafeRead rank px
        ry <- unsafeRead rank py
        case compare rx ry of
            LT -> unsafeWrite parent px py
            GT -> unsafeWrite parent py px
            EQ -> do
                unsafeWrite parent px py
                unsafeWrite rank py $ ry + 1

equivM :: UnionFindST s -> Int -> Int -> ST s Bool
equivM uf x y = (==) <$> findM uf x <*> findM uf y

countGroupM :: UnionFindST s -> ST s Int
countGroupM UF{..} = fix `flip` 0 `flip` 0 $ \loop !i !res ->
    if i < size
    then do
        p <- unsafeRead parent i
        if p == nothing
        then loop (i + 1) (res + 1)
        else loop (i + 1) res
    else return res

