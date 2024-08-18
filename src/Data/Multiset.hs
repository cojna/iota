{-# LANGUAGE TypeFamilies #-}

module Data.Multiset where

import Data.Coerce
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import GHC.Exts

newtype Multiset a = Multiset {getMultiset :: M.Map a Int}
    deriving (Eq)

instance (Show a, Ord a) => Show (Multiset a) where
    show = show . toList

instance (Ord a) => IsList (Multiset a) where
    type Item (Multiset a) = a
    fromList = fromListMS
    toList = toListMS

emptyMS :: Multiset a
emptyMS = coerce (M.empty @_ @Int)

singletonMS :: a -> Multiset a
singletonMS = coerce (flip (M.singleton @_ @Int) 1)

{- | /O(1)/

>>> replicateMS 3 1
[1,1,1]
>>> nullMS $ replicateMS 0 1
True
>>> nullMS $ replicateMS (-1) 1
True
-}
replicateMS :: Int -> a -> Multiset a
replicateMS = coerce ((M.filter (> 0) .) . flip (M.singleton @_ @Int))

{- | /O(n log n)/

>>> fromListMS [0,1,2,1,0]
[0,0,1,1,2]
-}
fromListMS :: (Ord a) => [a] -> Multiset a
fromListMS = L.foldl' (flip insertMS) emptyMS

{- | /O(n)/

>>> fromAscListMS [0,0,1,2]
[0,0,1,2]
-}
fromAscListMS :: (Eq a) => [a] -> Multiset a
fromAscListMS =
    Multiset
        . M.fromDistinctAscList
        . map (\g -> (NE.head g, NE.length g))
        . NE.group

{- | /O(n)/

>>> fromDistinctAscListMS [0,1,2]
[0,1,2]
-}
fromDistinctAscListMS :: [a] -> Multiset a
fromDistinctAscListMS =
    coerce (M.fromDistinctAscList @_ @Int . map (flip (,) 1))

{- | /O(n)/

>>> fromDescListMS [2,1,0,0]
[0,0,1,2]
-}
fromDescListMS :: (Eq a) => [a] -> Multiset a
fromDescListMS =
    Multiset
        . M.fromDistinctAscList
        . reverse
        . map (\g -> (NE.head g, NE.length g))
        . NE.group

{- | /O(n)/

>>> fromDistinctDescListMS [2,1,0]
[0,1,2]
-}
fromDistinctDescListMS :: [a] -> Multiset a
fromDistinctDescListMS =
    coerce (M.fromDistinctAscList @_ @Int . map (flip (,) 1) . reverse)

{- | /O(n)/

>>> toListMS (fromListMS [0,1,0,2])
[0,0,1,2]
-}
toListMS :: Multiset a -> [a]
toListMS = toAscListMS

{- | /O(n)/

>>> toAscListMS (fromListMS [0,1,0,2])
[0,0,1,2]
-}
toAscListMS :: Multiset a -> [a]
toAscListMS =
    concatMap @[] (\(k, x) -> replicate x k)
        . coerce (M.toAscList @_ @Int)

{- | /O(n)/

>>> toDescListMS (fromListMS [0,1,0,2])
[2,1,0,0]
-}
toDescListMS :: Multiset a -> [a]
toDescListMS =
    concatMap @[] (\(k, x) -> replicate x k)
        . coerce (M.toDescList @_ @Int)

-- | /O(log n)/
insertMS :: (Ord a) => a -> Multiset a -> Multiset a
insertMS = coerce (flip (M.insertWith @_ @Int (+)) 1)
{-# INLINE insertMS #-}

{- | /O(log n)/

>>> deleteMS 0 (fromList [0, 0, 1, 2])
[0,1,2]
>>> deleteMS 3 (fromList [0, 0, 1, 2])
[0,0,1,2]
-}
deleteMS :: (Ord a) => a -> Multiset a -> Multiset a
deleteMS = coerce (M.update @_ @Int (\y -> if y > 1 then Just $! y - 1 else Nothing))

{- | /O(log n)/

>>> deleteAllMS 0 (fromList [0, 0, 1, 2])
[1,2]
>>> deleteAllMS 3 (fromList [0, 0, 1, 2])
[0,0,1,2]
-}
deleteAllMS :: (Ord a) => a -> Multiset a -> Multiset a
deleteAllMS = coerce (M.delete @_ @Int)

-- | /O(log n)/
memberMS :: (Ord a) => a -> Multiset a -> Bool
memberMS = coerce (M.member @_ @Int)

-- | /O(log n)/
notMemberMS :: (Ord a) => a -> Multiset a -> Bool
notMemberMS = coerce (M.notMember @_ @Int)

{- | /O(log n)/

>>> countMS 0 (fromList [0, 0, 1, 2])
2
>>> countMS 1 (fromList [0, 0, 1, 2])
1
>>> countMS (-1) (fromList [0, 0, 1, 2])
0
-}
countMS :: (Ord a) => a -> Multiset a -> Int
countMS = coerce (M.findWithDefault @_ @Int 0)

{- | /O(log n)/

>>> lookupLTMS 1 (fromList [0, 0, 1, 1, 2, 2])
Just 0
>>> lookupLTMS 0 (fromList [0, 0, 1, 1, 2, 2])
Nothing
-}
lookupLTMS :: (Ord a) => a -> Multiset a -> Maybe a
lookupLTMS x = coerce (fmap fst . M.lookupLT @_ @Int x)

{- | /O(log n)/

>>> lookupGTMS 1 (fromList [0, 0, 1, 1, 2, 2])
Just 2
>>> lookupGTMS 2 (fromList [0, 0, 1, 1, 2, 2])
Nothing
-}
lookupGTMS :: (Ord a) => a -> Multiset a -> Maybe a
lookupGTMS x = coerce (fmap fst . M.lookupGT @_ @Int x)

{- | /O(log n)/

>>> lookupLEMS 1 (fromList [0, 0, 1, 1, 2, 2])
Just 1
>>> lookupLEMS 0 (fromList [0, 0, 1, 1, 2, 2])
Just 0
>>> lookupLEMS (-1) (fromList [0, 0, 1, 1, 2, 2])
Nothing
-}
lookupLEMS :: (Ord a) => a -> Multiset a -> Maybe a
lookupLEMS x = coerce (fmap fst . M.lookupLE @_ @Int x)

{- | /O(log n)/

>>> lookupGEMS 1 (fromList [0, 0, 1, 1, 2, 2])
Just 1
>>> lookupGEMS 2 (fromList [0, 0, 1, 1, 2, 2])
Just 2
>>> lookupGEMS 3 (fromList [0, 0, 1, 1, 2, 2])
Nothing
-}
lookupGEMS :: (Ord a) => a -> Multiset a -> Maybe a
lookupGEMS x = coerce (fmap fst . M.lookupGE @_ @Int x)

-- | /O(1)/
nullMS :: Multiset a -> Bool
nullMS = coerce (M.null @_ @Int)

{- | /O(n)/

>>> sizeMS (fromList [0, 0, 1, 2])
4
>>> sizeMS emptyMS
0
-}
sizeMS :: Multiset a -> Int
sizeMS = coerce (M.foldl' @Int (+) 0)

{- | /O(1)/
>>> distinctSizeMS (fromList [1,1,1,2,2,3])
3
-}
distinctSizeMS :: Multiset a -> Int
distinctSizeMS = coerce (M.size @_ @Int)

{- | /O(log n)/

>>> findMinMS (fromList [0, 0, 1, 2])
0
>>> findMinMS emptyMS
*** Exception: Map.findMin: empty map has no minimal element
-}
findMinMS :: Multiset a -> a
findMinMS = coerce (fst . M.findMin @_ @Int)

{- | /O(log n)/

>>> findMaxMS (fromList [0, 0, 1, 2])
2

>>> findMaxMS emptyMS
*** Exception: Map.findMax: empty map has no maximal element
-}
findMaxMS :: Multiset a -> a
findMaxMS = coerce (fst . M.findMax @_ @Int)

{- | /O(log n)/

>>> deleteMinMS (fromList [0, 0, 1, 2])
[0,1,2]
>>> deleteMinMS emptyMS
[]
-}
deleteMinMS :: Multiset a -> Multiset a
deleteMinMS =
    coerce (M.updateMin @Int (\x -> if x > 1 then Just $! x - 1 else Nothing))

{- | /O(log n)/

>>> deleteMaxMS (fromList [0, 1, 2, 2])
[0,1,2]
>>> deleteMaxMS emptyMS
[]
-}
deleteMaxMS :: Multiset a -> Multiset a
deleteMaxMS =
    coerce (M.updateMax @Int (\x -> if x > 1 then Just $! x - 1 else Nothing))

{- | /O(log n)/

>>> deleteFindMinMS (fromList [0, 0, 1, 2])
(0,[0,1,2])
>>> deleteFindMinMS emptyMS
*** Exception: Map.deleteFindMin: can not return the minimal element of an empty map
-}
deleteFindMinMS :: forall a. (Ord a) => Multiset a -> (a, Multiset a)
deleteFindMinMS = coerce (found . M.deleteFindMin)
    where
        found :: ((a, Int), M.Map a Int) -> (a, M.Map a Int)
        found ((k, x), m)
            | x > 1 = case M.insert k (x - 1) m of
                m' -> (k, m')
            | otherwise = (k, m)

{- | /O(log n)/

>>> deleteFindMaxMS (fromList [0, 1, 2, 2])
(2,[0,1,2])

>>> deleteFindMaxMS emptyMS
*** Exception: Map.deleteFindMax: can not return the maximal element of an empty map
-}
deleteFindMaxMS :: forall a. (Ord a) => Multiset a -> (a, Multiset a)
deleteFindMaxMS = coerce (found . M.deleteFindMax)
    where
        found :: ((a, Int), M.Map a Int) -> (a, M.Map a Int)
        found ((k, x), m)
            | x > 1 = case M.insert k (x - 1) m of
                m' -> (k, m')
            | otherwise = (k, m)

{- | /O(log n)/

>>> minViewMS (fromList [0, 0, 1, 2])
Just (0,[0,1,2])

>>> minViewMS emptyMS
Nothing
-}
minViewMS :: forall a. (Ord a) => Multiset a -> Maybe (a, Multiset a)
minViewMS = coerce (maybe Nothing just . M.minViewWithKey)
    where
        just :: ((a, Int), M.Map a Int) -> Maybe (a, Multiset a)
        just ((k, x), m)
            | x > 1 = case M.insert k (x - 1) m of
                m' -> coerce (Just (k, m'))
            | otherwise = coerce (Just (k, m))

{- | /O(log n)/

>>> maxViewMS (fromList [0, 1, 2, 2])
Just (2,[0,1,2])

>>> maxViewMS emptyMS
Nothing
-}
maxViewMS :: forall a. (Ord a) => Multiset a -> Maybe (a, Multiset a)
maxViewMS = coerce (maybe Nothing just . M.maxViewWithKey)
    where
        just :: ((a, Int), M.Map a Int) -> Maybe (a, Multiset a)
        just ((k, x), m)
            | x > 1 = case M.insert k (x - 1) m of
                m' -> coerce (Just (k, m'))
            | otherwise = coerce (Just (k, m))

{- | /O(log n)/

>>> splitMS 1 (fromList [0, 0, 1, 2])
([0,0],[2])
>>> splitMS 0 (fromList [0, 0, 1, 2])
([],[1,2])
>>> splitMS (-1) (fromList [0, 0, 1, 2])
([],[0,0,1,2])
-}
splitMS :: (Ord a) => a -> Multiset a -> (Multiset a, Multiset a)
splitMS = coerce (M.split @_ @Int)

{- | /O(log n)/

>>> splitLookupMS 1 (fromList [0, 0, 1, 2])
([0,0],[1],[2])
>>> splitLookupMS 0 (fromList [0, 0, 1, 2])
([],[0,0],[1,2])
>>> splitLookupMS (-1) (fromList [0, 0, 1, 2])
([],[],[0,0,1,2])
-}
splitLookupMS :: (Ord a) => a -> Multiset a -> (Multiset a, [a], Multiset a)
splitLookupMS k h = case coerce (M.splitLookup @_ @Int) k h of
    (l, Just cnt, r) -> (l, replicate cnt k, r)
    (l, Nothing, r) -> (l, [], r)
