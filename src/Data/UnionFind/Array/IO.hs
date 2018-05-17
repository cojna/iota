{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}

module Data.UnionFind.Array.IO where

import           Control.Monad
import           Data.Array.Base
import           Data.Array.IO
import           Data.Function

nothing :: Int
nothing = -1
{-# INLINE nothing #-}

type Parent = Int
type Rank = Int
data UnionFindIO = UF
    { size   :: !Int
    , parent :: !(IOUArray Int Parent)
    , rank   :: !(IOUArray Int Rank)
    }

newUnionFindIO :: Int -> IO UnionFindIO
newUnionFindIO n = UF n
    <$> newArray (0, n - 1) nothing
    <*> newArray (0, n - 1) 0

findM :: UnionFindIO -> Int -> IO Parent
findM uf@UF{..} x = do
    px <- unsafeRead parent x
    if px == nothing
    then return x
    else do
        ppx <- findM uf px
        unsafeWrite parent x ppx
        return ppx

uniteM :: UnionFindIO -> Int -> Int -> IO ()
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

equivM :: UnionFindIO -> Int -> Int -> IO Bool
equivM uf x y = (==) <$> findM uf x <*> findM uf y

countGroupM :: UnionFindIO -> IO Int
countGroupM UF{..} = fix `flip` 0 `flip` 0 $ \loop !i !res ->
    if i < size
    then do
        p <- unsafeRead parent i
        if p == nothing
        then loop (i + 1) (res + 1)
        else loop (i + 1) res
    else return res

