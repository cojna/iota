module Algorithm.Search.SternBrocot where

import GHC.Real

{- |
>>> binarySearchRational 100 (\x -> compare (x * x) 2)
(140 % 99,99 % 70)
>>> map (fromRational @Double) [140 % 99,  99 % 70]
[1.4141414141414141,1.4142857142857144]
>>> binarySearchRational (10^10) (`compare`(1 % (10^10)))
(1 % 10000000000,1 % 9999999999)
>>> binarySearchRational (10^10) (`compare`((10^10-1) % (10^10)))
(9999999999 % 10000000000,1 % 1)
-}
binarySearchRational ::
  Integer ->
  (Rational -> Ordering) ->
  (Rational, Rational)
binarySearchRational n approx = go (0 :% 1) (1 :% 0)
  where
    go l@(lp :% lq) r@(rp :% rq)
      | denominator m > n = (l, r)
      | otherwise = case approx m of
          LT ->
            flip go r
              . last
              . (m :)
              . takeWhile
                ( \m' ->
                    denominator m' <= n && approx m' == LT
                )
              . map (\i -> (lp + i * rp) :% (lq + i * rq))
              $ iterate (* 2) 2
          EQ -> go m r
          GT ->
            go l
              . last
              . (m :)
              . takeWhile
                ( \m' ->
                    denominator m' <= n && approx m' == GT
                )
              . map (\i -> (i * lp + rp) :% (i * lq + rq))
              $ iterate (* 2) 2
      where
        !m = (lp + rp) :% (lq + rq)

{- |
>>> binarySearchMinRational 100 (1 <)
101 % 100
>>> binarySearchMinRational 100 (1 <=)
1 % 1
-}
binarySearchMinRational :: Integer -> (Rational -> Bool) -> Rational
binarySearchMinRational n f =
  snd . binarySearchRational n $ \x ->
    if f x then GT else LT

{- |
>>> binarySearchMaxRational 100 (< 1)
99 % 100
>>> binarySearchMaxRational 100 (<= 1)
1 % 1
-}
binarySearchMaxRational :: Integer -> (Rational -> Bool) -> Rational
binarySearchMaxRational n f =
  fst . binarySearchRational n $ \x ->
    if f x then LT else GT
