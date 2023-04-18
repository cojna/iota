{-# LANGUAGE BangPatterns #-}

module Math.Modulus where

import Control.Monad
import Data.Bits
import qualified Data.Foldable as F

{- |
>>> powMod 2 0 1000000007
1
>>> powMod 0 0 1000000007
1
>>> powMod 2 1000000005 1000000007
500000004
>>> powMod 2 (-1) 1000000007
500000004
>>> powMod 123456789 998244353 998244353
123456789
>>> powMod (-2) 2 1000000007
4
>>> powMod (-2) 3 1000000007
999999999
-}
powMod :: (Integral a) => a -> Int -> a -> a
powMod x n m
  | n > 0 = go 1 x n
  | n == 0 = 1
  | otherwise = go 1 (recipMod x m) (- n)
  where
    go !acc !y !i
      | i .&. 1 == 0 = go acc (y * y `rem` m) (unsafeShiftR i 1)
      | i == 1 = acc * y `mod` m
      | otherwise = go (acc * y `rem` m) (y * y `rem` m) (unsafeShiftR (i - 1) 1)
{-# INLINE powMod #-}

{- |
>>> recipMod 2 1000000007
500000004
>>> recipMod 10 1000000007
700000005
>>> recipMod 0 1000000007
0

prop> \x m -> not (m >= 1) || mod (x * recipMod x m) m == mod (gcd x m) m
+++ OK, passed 100 tests.
-}
recipMod :: (Integral a) => a -> a -> a
recipMod x m = go (mod x m) m 1 0
  where
    go !a !b !u !v
      | b > 0 = case a `quot` b of
          q -> go b (a - (q * b)) v (u - (q * v))
      | otherwise = u `mod` m
{-# INLINE recipMod #-}

{- | Extended Euclidean algorithm

@(x, y, g) = extGCD a b (a * x + b * y = g)@

>>> extGCD 3 5
(2,-1,1)
>>> extGCD 4 6
(-1,1,2)

prop> \a b -> not (a > 0 && b > 0) || let (_, _ , g) = extGCD a b in g == gcd a b
+++ OK, passed 100 tests.

prop> \a b -> not (a > 0 && b > 0) || let (x, y, g) = extGCD a b in a * x + b * y == g
+++ OK, passed 100 tests.

prop> \a b -> not (a > 0 && b > 0) || let (x, y, g) = extGCD a b in abs x <= div b g && abs y <= div a g
+++ OK, passed 100 tests.
-}
extGCD :: (Integral a) => a -> a -> (a, a, a)
extGCD a0 b0 = go a0 b0 1 0
  where
    go !a !b !u !v
      | b > 0 = case quot a b of
          q -> go b (a - (q * b)) v (u - (q * v))
      | otherwise = (u, quot (a - a0 * u) b0, a)
{-# INLINE extGCD #-}

{- | Solve @a * x + b * y == c@

If multiple solutions exists then return the minimum non-negative @x@.

If @a \/= 0, b \/= 0, a * x + b * y == c@
then @a (x - k * b \/ g) + b * (y + k * a \/ g) == c@

>>> linearDiophantine 3 5 1
Just (2,-1)
>>> linearDiophantine 3 5 3
Just (1,0)
>>> linearDiophantine 4 6 2
Just (2,-1)
>>> linearDiophantine 4 6 3
Nothing
prop> \a b c -> maybe True (\(x, y) -> a * x + b * y == c) $ linearDiophantine a b c
+++ OK, passed 100 tests.
-}
linearDiophantine :: (Integral a) => a -> a -> a -> Maybe (a, a)
linearDiophantine _ _ 0 = Just (0, 0)
linearDiophantine 0 0 _ = Nothing
linearDiophantine 0 b c
  | (q, 0) <- divMod c b = Just (0, q)
  | otherwise = Nothing
linearDiophantine a 0 c
  | (q, 0) <- divMod c a = Just (q, 0)
  | otherwise = Nothing
linearDiophantine a b c | c < 0 = linearDiophantine (-a) (-b) (-c)
linearDiophantine a b c
  | mod c g > 0 = Nothing
  | otherwise = Just (x, y)
  where
    (!x0, !_, !g) = extGCD (abs a) (abs b)
    b' = quot (abs b) g
    c' = quot c g -- c > 0
    x = mod (signum a * c' * x0) b'
    y = div (c - a * x) b

{- | Chinese Remainder Theorem

@x = r0 (mod m0), x = r1 (mod m1) ==> x (mod (lcm m0 m1))@

>>> crt (10, 20) (10, 30)
Just (10,60)
>>> crt (10, 20) (10, 20)
Just (10,20)
>>> crt (10, 20) (11, 20)
Nothing
-}
crt :: (Integral a) => (a, a) -> (a, a) -> Maybe (a, a)
crt (!r0, !m0) (!r1, !m1)
  | mod (r1 - r0) g == 0 = Just (r, m)
  | otherwise = Nothing
  where
    -- m0 * p + m1 * q == g
    (p, _q, g) = extGCD m0 m1
    !m = m0 * quot m1 g
    !r = mod (r0 + m0 * quot (r1 - r0) g * p) m

{- | * @p0@, @p1@ are prime
   * @p0 /= p1@ or @r0 == r1@

>>> crt' (2,3) (3,5)
8
>>> crt' (3,5) (3,7)
3
>>> crt' (3,7) (3,7)
3
-}
crt' :: (Integral a) => (a, a) -> (a, a) -> a
crt' (r0, p0) (r1, p1) =
  mod (r0 + p0 * recipMod p0 p1 `rem` (p0 * p1) * (r1 - r0)) (p0 * p1)

{- |
>>> crts [(20,30),(30,50),(20,70)]
Just (230,1050)
>>> crts []
Just (0,1)
>>> crts [(1, 10), (2, 20), (3, 30)]
Nothing
-}
crts :: (Integral a) => [(a, a)] -> Maybe (a, a)
crts cs = foldr ((>=>) . crt) return cs (0, 1)

{- |
@x (mod m) (x = r[i] (mod m[i]))@

@gcd m[i] m[j] == 1@

/O(n^2)/

>>> garner [(2, 3), (3, 5), (2, 7)] 999
23
>>> garner [(2, 3), (3, 5), (2, 7)] 10
3
-}
garner :: (Integral a) => [(a, a)] -> a -> a
garner rms m0 = go [] rms
  where
    ms = map snd rms
    go cs ((r, m) : rest) = go (cs ++ [c]) rest
      where
        -- a + b * c = r (mod m)
        (a, b) = F.foldl' (step m) (0, 1) $ zip cs ms
        !c = rem (mod (r - a) m * recipMod b m) m
    go cs [] = fst . F.foldl' (step m0) (0, 1) $ zip cs ms

    step m (!s, !p) (ci, mi) = (s', p')
      where
        !s' = rem (s + rem (ci * p) m) m
        !p' = rem (p * mi) m
