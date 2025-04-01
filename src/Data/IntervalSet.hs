{-# LANGUAGE TypeFamilies #-}

module Data.IntervalSet where

import qualified Data.Foldable as F
import qualified Data.IntMap.Strict as IM
import GHC.Exts

newtype IntervalSet = IntervalSet (IM.IntMap Int)
  deriving (Eq)

instance Show IntervalSet where
  show (IntervalSet s) = show $ IM.toAscList s

emptyIS :: IntervalSet
emptyIS = IntervalSet IM.empty

singletonIS :: Int -> IntervalSet
singletonIS x = IntervalSet (IM.singleton x x)

{- |
>>> intervalIS (0, 1)
[(0,1)]
>>> intervalIS (1, 0)
[]
-}
intervalIS :: (Int, Int) -> IntervalSet
intervalIS (l, r)
  | l <= r = IntervalSet (IM.singleton l r)
  | otherwise = emptyIS

{- |
>>> insertIS 2 $ fromListIS [(0,1),(3,4)]
[(0,4)]
>>> insertIS 1 $ fromListIS [(0,1)]
[(0,1)]
-}
insertIS :: Int -> IntervalSet -> IntervalSet
insertIS x (IntervalSet s) = IntervalSet $ case IM.lookupLE x s of
  Just (l, r)
    | x <= r -> s
    | otherwise -> case IM.lookup (x + 1) s of
        Just r'
          | x == r + 1 -> IM.delete (x + 1) $ IM.insert l r' s
          | otherwise -> IM.delete (x + 1) $ IM.insert x r' s
        Nothing
          | x == r + 1 -> IM.insert l x s
          | otherwise -> IM.insert x x s
  Nothing -> case IM.lookup (x + 1) s of
    Just r' -> IM.delete (x + 1) $ IM.insert x r' s
    Nothing -> IM.insert x x s

{- |
>>> insertIntervalIS (2,4) $ fromListIS [(1,1),(3,3),(5,5)]
[(1,5)]
-}
insertIntervalIS :: (Int, Int) -> IntervalSet -> IntervalSet
insertIntervalIS (l, r) (IntervalSet s)
  | l <= r = IntervalSet (go s)
  | otherwise = IntervalSet s
  where
    !l' = case IM.lookupLE l s of
      Just (ll, lr) | l <= lr + 1 -> ll
      _ -> l
    !r' = case IM.lookupLE r s of
      Just (_, rr) | r <= rr -> rr
      _ -> IM.findWithDefault r (r + 1) s
    go im = case IM.lookupLE r' im of
      Just (kl, _)
        | l' <= kl -> go $ IM.delete kl im
      _ -> IM.insert l' r' im

{- |
>>> deleteIS 1 $ fromListIS [0..2]
[(0,0),(2,2)]
-}
deleteIS :: Int -> IntervalSet -> IntervalSet
deleteIS x (IntervalSet s) = IntervalSet $ case IM.lookupLE x s of
  Just (l, r) | x <= r -> fl l $ fr r s
  _ -> s
  where
    fl l im
      | l /= x = IM.insert l (x - 1) im
      | otherwise = IM.delete l im
    fr r im
      | r /= x = IM.insert (x + 1) r im
      | otherwise = im

{- |
>>> deleteIntervalIS (1,7) $ fromListIS [(0,1),(3,5),(7,8)]
[(0,0),(8,8)]
>>> deleteIntervalIS (1,1) $ fromListIS [(0,0)]
[(0,0)]
-}
deleteIntervalIS :: (Int, Int) -> IntervalSet -> IntervalSet
deleteIntervalIS (l, r) (IntervalSet s)
  | l <= r = IntervalSet $ go s
  | otherwise = (IntervalSet s)
  where
    go !im = case IM.lookupLE r im of
      Just (l', r')
        | l <= l' ->
            if r < r'
              then go . IM.delete l' $ IM.insert (r + 1) r' im
              else go $ IM.delete l' im
        | r < r' -> IM.insert (r + 1) r' $ IM.insert l' (l - 1) im
        | l <= r' -> IM.insert l' (l - 1) im
        | otherwise -> im
      Nothing -> im

{- |
>>> memberIS 0 (fromListIS [(-1,1)])
True
>>> memberIS 1 (fromListIS [(-1,1)])
True
>>> memberIS 2 (fromListIS [(-1,1)])
False
-}
memberIS :: Int -> IntervalSet -> Bool
memberIS x (IntervalSet s) = case IM.lookupLE x s of
  Just (_, r) -> x <= r
  Nothing -> False

{- |
>>> lookupIS (-2) $ intervalIS (-1,1)
Nothing
>>> lookupIS (-1) $ intervalIS (-1,1)
Just (-1,1)
>>> lookupIS 0 $ intervalIS (-1,1)
Just (-1,1)
>>> lookupIS 1 $ intervalIS (-1,1)
Just (-1,1)
>>> lookupIS 2 $ intervalIS (-1,1)
Nothing
-}
lookupIS :: Int -> IntervalSet -> Maybe (Int, Int)
lookupIS x (IntervalSet s) = case IM.lookupLE x s of
  Just (l, r) | x <= r -> Just (l, r)
  _ -> Nothing

{- |
>>> lookupIntervalIS (1,8) $ fromList [(0,2),(4,5),(7,9)]
[(0,2),(4,5),(7,9)]
>>> lookupIntervalIS (3,6) $ fromList [(0,2),(4,5),(7,9)]
[(4,5)]
>>> lookupIntervalIS (3,3) $ fromList [(0,2),(4,5),(7,9)]
[]
>>> lookupIntervalIS (0, 1) $ fromList [(1, 2)]
[(1,2)]
>>> lookupIntervalIS (0, 1) $ fromList [(-1, 0)]
[(-1,0)]
-}
lookupIntervalIS :: (Int, Int) -> IntervalSet -> [(Int, Int)]
lookupIntervalIS (l0, r0) (IntervalSet s)
  | l0 <= r0 = case IM.lookupLE l0 s of
      Just (l, r) | l0 <= r -> (l, r) : go r
      _ -> go l0
  | otherwise = []
  where
    go r = case IM.lookupGT r s of
      Just (l', r')
        | l' <= r0 -> (l', r') : go r'
      _ -> []

{- | minimum excluded value

>>> mex $ fromListIS [(0,1),(3,5)]
2
>>> mex $ intervalIS (1, 5)
0
-}
mex :: IntervalSet -> Int
mex (IntervalSet s) = case IM.lookupLE 0 s of
  Just (_, r) | 0 <= r -> r + 1
  _ -> 0

fromListIS :: [(Int, Int)] -> IntervalSet
fromListIS = F.foldl' (flip insertIntervalIS) emptyIS

toListIS :: IntervalSet -> [(Int, Int)]
toListIS (IntervalSet s) = IM.toAscList s

instance IsList IntervalSet where
  type Item IntervalSet = (Int, Int)
  fromList = fromListIS
  toList = toListIS
