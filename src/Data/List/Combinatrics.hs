module Data.List.Combinatrics where

import qualified Data.List as L

{- |
>>> combinations 2 [1..3]
[[1,2],[1,3],[2,3]]
-}
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations 1 xs = map (: []) xs
combinations k (x : xs) = map (x :) (combinations (k - 1) xs) ++ combinations k xs
combinations _ [] = []

{- |
>>> allPairs [1..4]
[(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]
>>> allPairs []
[]
-}
allPairs :: [a] -> [(a, a)]
allPairs xs = do
  y : ys <- L.tails xs
  map ((,) y) ys

{- |
>>> choice [1..3]
[(1,[2,3]),(2,[1,3]),(3,[1,2])]
-}
choice :: [a] -> [(a, [a])]
choice xs0 = do
  (xs, y : ys) <- zip (L.inits xs0) (L.tails xs0)
  return (y, xs ++ ys)

{- |
>>> pairPartitions [1..4]
[[(1,2),(3,4)],[(1,3),(2,4)],[(1,4),(2,3)]]
>>> pairPartitions [1..3]
[[(1,2)],[(1,3)],[(2,3)]]
>>> length (pairPartitions[1..16])
2027025
>>> product[1,3..15]
2027025
-}
pairPartitions :: (Eq a) => [a] -> [[(a, a)]]
pairPartitions [] = []
pairPartitions [_] = []
pairPartitions xxs0@(_ : xs0)
  | even n = go xxs0
  | otherwise = go xxs0 <> go xs0
  where
    n = length xxs0
    go [_] = [[]]
    go (x : xs) = do
      y <- xs
      map ((x, y) :) (go (L.delete y xs))
    go [] = [[]]

{- |
>>> allPartitions [1..3]
[[[1],[2],[3]],[[1],[2,3]],[[1,2],[3]],[[1,3],[2]],[[1,2,3]]]
>>> length (allPartitions [1..12])
4213597
-}
allPartitions :: (Eq a) => [a] -> [[[a]]]
allPartitions xs0 = go xs0
  where
    go (x : xs) = do
      ys <- L.subsequences xs
      zs <- go (xs L.\\ ys)
      pure $ (x : ys) : zs
    go [] = [[]]
