module Data.IntHeap.MaxTopK where

import qualified Data.List as L

import Data.IntHeap

data MaxTopK = MaxTopK !IntHeap !Int !Int !IntHeap
  deriving (Eq)

instance Show MaxTopK where
  show (MaxTopK minHeapL minL maxR maxHeapR) =
    show
      ( reverse $ minL : toAscListIH minHeapL
      , maxR : toDescListIH maxHeapR
      )

{- | /O(1)/

>>> viewMaxTopK (buildMaxTopK 3 [0..9])
7
-}
viewMaxTopK :: MaxTopK -> Int
viewMaxTopK (MaxTopK _ minL _ _) = minL

{- | /O(k)/

>>> viewMaxTopKList (buildMaxTopK 3 [0..9])
[9,8,7]
-}
viewMaxTopKList :: MaxTopK -> [Int]
viewMaxTopKList (MaxTopK minHeapL minL _ _) = toDescListIH minHeapL ++ [minL]

newMaxTopK :: Int -> MaxTopK
newMaxTopK k = MaxTopK (replicateIH (k - 1) minBound) minBound minBound emptyIH

{- | /O(n log n)/

>>> buildMaxTopK 4 [0..4]
([4,3,2,1],[0])
>>> buildMaxTopK 4 [0..3]
([3,2,1,0],[-9223372036854775808])
>>> buildMaxTopK 4 [0..2]
([2,1,0,-9223372036854775808],[-9223372036854775808])
-}
buildMaxTopK :: Int -> [Int] -> MaxTopK
buildMaxTopK k xs =
  buildMaxTopKFromDescList k $
    L.sortBy (flip compare) xs

{- | /O(n)/

>>> buildMaxTopKFromDescList 3 [2,2,1,1,0,0]
([2,2,1],[1,0,0])
-}
buildMaxTopKFromDescList :: Int -> [Int] -> MaxTopK
buildMaxTopKFromDescList k xs = case splitAt (k - 1) xs of
  (ls, minL : maxR : rs) -> MaxTopK (fromDescListIH ls) minL maxR (fromDescListIH rs)
  (ls, [minL]) -> MaxTopK (fromDescListIH ls) minL minBound emptyIH
  (ls, []) ->
    MaxTopK
      (fromDescListIH . take (k - 1) $ ls ++ repeat minBound)
      minBound
      minBound
      emptyIH

{- | /O(min(n,W))/

>>> maxTop4 = buildMaxTopK 4 [0..4]
>>> maxTop4
([4,3,2,1],[0])
>>> insertMaxTopK 999 maxTop4
(Just 1,([999,4,3,2],[1,0]))
>>> insertMaxTopK (-999) maxTop4
(Nothing,([4,3,2,1],[0,-999]))
>>> insertMaxTopK 1 maxTop4
(Nothing,([4,3,2,1],[1,0]))
-}
insertMaxTopK ::
  Int ->
  MaxTopK ->
  -- | Maybe deleted
  (Maybe Int, MaxTopK)
insertMaxTopK x (MaxTopK minHeapL minL maxR maxHeapR)
  | x <= minL =
    ( Nothing
    , MaxTopK minHeapL minL (max x maxR) (insertIH (min x maxR) maxHeapR)
    )
  | otherwise =
    case deleteFindMinIH (insertIH x minHeapL) of
      (minL', minHeapL') ->
        ( Just minL
        , MaxTopK minHeapL' minL' minL (insertIH maxR maxHeapR)
        )

{- | /O(min(n,W))/

>>> maxTop3 = buildMaxTopK 3 [0,1,1,2,2]
>>> maxTop3
([2,2,1],[1,0])
>>> deleteMaxTopK 2 maxTop3
(Just 1,([2,1,1],[0]))
>>> deleteMaxTopK 1 maxTop3
(Nothing,([2,2,1],[0]))
>>> deleteMaxTopK 0 maxTop3
(Nothing,([2,2,1],[1]))
>>> deleteMaxTopK 999 maxTop3
(Nothing,([2,2,1],[1,0]))
>>> deleteMaxTopK (-999) maxTop3
(Nothing,([2,2,1],[1,0]))
>>> deleteMaxTopK 2 $ buildMaxTopK 3 [2,2,2,2,2]
(Nothing,([2,2,2],[2]))
>>> deleteMaxTopK 0 $ buildMaxTopK 1 [0]
(Just (-9223372036854775808),([-9223372036854775808],[-9223372036854775808]))
-}
deleteMaxTopK ::
  Int ->
  MaxTopK ->
  -- | Maybe inserted
  (Maybe Int, MaxTopK)
deleteMaxTopK x (MaxTopK minHeapL minL maxR maxHeapR)
  | x < maxR =
    ( Nothing
    , MaxTopK minHeapL minL maxR $ deleteIH x maxHeapR
    )
  | x == maxR = case deleteFindMax maxHeapR of
    (maxR', maxHeapR') ->
      ( Nothing
      , MaxTopK minHeapL minL maxR' maxHeapR'
      )
  | x == minL = case deleteFindMax maxHeapR of
    (maxR', maxHeapR') ->
      ( Just maxR
      , MaxTopK minHeapL maxR maxR' maxHeapR'
      )
  | notMemberIH x minHeapL =
    ( Nothing
    , MaxTopK minHeapL minL maxR maxHeapR
    )
  | otherwise = case deleteFindMax maxHeapR of
    (maxR', maxHeapR') ->
      ( Just maxR
      , MaxTopK (insertIH minL $ deleteIH x minHeapL) maxR maxR' maxHeapR'
      )
  where
    deleteFindMax heap = case maxViewIH heap of
      Just (mm, heap') -> (mm, heap')
      Nothing -> (minBound, heap)

{- | /O(min(n,W))/

>>> maxTop3 = buildMaxTopK 3 [0,1,1,2,2]
>>> maxTop3
([2,2,1],[1,0])
>>> exchangeMaxTopK 2 999 maxTop3
(Just (999,2),([999,2,1],[1,0]))
>>> exchangeMaxTopK 1 999 maxTop3
(Just (999,1),([999,2,2],[1,0]))
>>> exchangeMaxTopK 0 999 maxTop3
(Just (999,1),([999,2,2],[1,1]))
>>> exchangeMaxTopK 2 1 maxTop3
(Just (1,2),([2,1,1],[1,0]))
>>> exchangeMaxTopK 0 1 maxTop3
(Nothing,([2,2,1],[1,1]))
>>> exchangeMaxTopK 1 1 maxTop3
(Nothing,([2,2,1],[1,0]))
>>> exchangeMaxTopK 999 999999 maxTop3
(Nothing,([2,2,1],[1,0]))
>>> exchangeMaxTopK 0 999 $ buildMaxTopK 1 [0, 0]
(Just (999,0),([999],[0]))
-}
exchangeMaxTopK ::
  -- | old
  Int ->
  -- | new
  Int ->
  MaxTopK ->
  -- | Maybe (inserted, deleted)
  (Maybe (Int, Int), MaxTopK)
exchangeMaxTopK old new maxTopK@(MaxTopK minHeapL minL maxR maxHeapR)
  | minL /= old
    , maxR /= old
    , old < minL || notMemberIH old minHeapL
    , old > maxR || notMemberIH old maxHeapR =
    (Nothing, maxTopK)
  | old == new = (Nothing, maxTopK)
exchangeMaxTopK old new maxTopK = case insertMaxTopK new maxTopK of
  (Nothing, maxTopK') -> case deleteMaxTopK old maxTopK' of
    (Nothing, maxTopK'') -> (Nothing, maxTopK'')
    (Just inserted, maxTopK'') -> (Just (inserted, old), maxTopK'')
  (Just deleted, maxTopK') -> case deleteMaxTopK old maxTopK' of
    (Nothing, maxTopK'') -> (Just (new, deleted), maxTopK'')
    (Just _inserted, maxTopK'') -> (Just (new, old), maxTopK'')
