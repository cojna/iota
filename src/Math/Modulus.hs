{-# LANGUAGE BangPatterns #-}

module Math.Modulus where

import           Control.Monad
import           Data.Bits
import qualified Data.Foldable           as F
import           Data.Function
import qualified Data.IntMap.Strict      as IM

import           System.Random.XoRoShiRo

-- |
-- >>> powMod 2 0 1000000007
-- 1
-- >>> powMod 0 0 1000000007
-- 1
-- >>> powMod 2 1000000005 1000000007
-- 500000004
-- >>> powMod 2 (-1) 1000000007
-- 500000004
-- >>> powMod 123456789 998244353 998244353
-- 123456789
-- >>> powMod (-2) 2 1000000007
-- 4
-- >>> powMod (-2) 3 1000000007
-- 999999999
powMod :: (Integral a) => a -> Int -> a -> a
powMod x n m
    | n > 0 = go 1 x n
    | n == 0 = 1
    | otherwise = go 1 (recipMod x m) (-n)
  where
    go !acc !y !i
        | i .&. 1 == 0 = go acc (y * y `rem` m) (unsafeShiftR i 1)
        | i == 1 = acc * y `mod` m
        | otherwise = go (acc * y `rem` m) (y * y `rem` m) (unsafeShiftR (i - 1) 1)
{-# INLINE powMod #-}

-- |
-- >>> recipMod 2 1000000007
-- 500000004
-- >>> recipMod 10 1000000007
-- 700000005
recipMod :: (Integral a) => a -> a -> a
recipMod x m = go x m 1 0
  where
    go !a !b !u !v
        | b > 0 = case a `quot` b of
            q -> go b (a - (q * b)) v (u - (q * v))
        | otherwise = u `mod` m
{-# INLINE recipMod #-}

-- |
-- Baby-step Giant-step
--
-- @a^x = b (mod p)@ p is prime
--
-- /O(sqrt P * log P)/
--
-- >>> logMod 3 27 998244353
-- Just 3
-- >>> logMod 3 123456789 998244353
-- Just 772453214
-- >>> logMod 1 2 1000000007
-- Nothing
logMod :: Int -> Int -> Int -> Maybe Int
logMod a b p = go 0 b
  where
    !sqrtP = ceiling . sqrt $ fromIntegral p
    !g = powMod a (-sqrtP) p
    babyStep x = a * x `rem` p
    giantStep x = g * x `rem` p

    table :: IM.IntMap Int
    !table = IM.fromList $ zip (iterate babyStep 1) [0..sqrtP]

    go !i !x
        | i < sqrtP = case IM.lookup x table of
            Just j  -> Just $! i * sqrtP + j
            Nothing -> go (i + 1) $ giantStep x
        | otherwise = Nothing

-- | Legendre symbol (Euler's criterion)
--
-- p is /odd/ prime
legendreSymbol :: Int -> Int -> Int
legendreSymbol a p = powMod a (quot (p - 1) 2) p

-- |
-- @x^2 = a (mod p)@ p is prime
--
-- >>> sqrtMod 2 1000000007
-- [59713600,940286407]
-- >>> sqrtMod 3 1000000007
-- [82062379,917937628]
-- >>> sqrtMod 4 1000000007
-- [2,1000000005]
-- >>> sqrtMod 5 1000000007
-- []
sqrtMod :: Int -> Int -> [Int]
sqrtMod 0 _ = [0]
sqrtMod a 2 = [mod a 2]
sqrtMod a p = case legendreSymbol a p of
    0 -> [0]
    1 | rem p 4 == 3 -> withConjugate $ powMod a (quot (p + 1) 4) p
      | rem p 4 == 1 -> withConjugate $ cipolla (mod a p) p
    x | x == p - 1 -> []
    _ -> error $ "sqrtMod: unreachable (a, p) = " <> show (a, p)
  where
    withConjugate !x =
        let !x' = p - x
        in [min x x', max x x']

-- |
-- @cipolla a p ^ 2 = a (mod p)@ p is odd prime, (a | p) = 1
--
-- >>> cipolla 9 998244353
-- 3
-- >>> cipolla 9 1000000007
-- 1000000004
cipolla :: Int -> Int -> Int
cipolla a p = fst $ pow (ns, 1) (quot (p + 1) 2)
  where
    !ns = withRNG $ \rng -> do
        fix $ \loop -> do
            !x <- flip mod p <$> nextInt rng
            case legendreSymbol (x *% x -% a) p of
                0 -> loop
                1 -> loop
                _ -> pure x
    !ww = ns *% ns -% a

    mul (x0, y0) (x1, y1) = (x', y')
      where
        !x' = x0 *% x1 +% y0 *% y1 *% ww
        !y' = x0 *% y1 +% y0 *% x1

    pow _ 0 = (1, 0)
    pow xy n = go (1, 0) xy n
      where
        go !acc !xy !i
            | i .&. 1 == 0 = go acc (mul xy xy) (unsafeShiftR i 1)
            | i == 1 = mul acc xy
            | otherwise = go (mul acc xy) (mul xy xy) (unsafeShiftR (i - 1) 1)

    infixl 7 *%
    infixl 6 +%, -%

    x +% y = case x + y of
        r | r < p -> r
          | otherwise -> r - p
    {-# INLINE (+%) #-}

    x -% y = case x - y of
        r | r < 0 -> r + p
          | otherwise -> r
    {-# INLINE (-%) #-}

    x *% y = x * y `rem` p
    {-# INLINE (*%) #-}

-- |
-- (x, y, g) = extGCD a b (a * x + b * y = g)
extGCD :: (Integral a) => a -> a -> (a, a, a)
extGCD a b = go a b 1 0
  where
    go !a !b !u !v
        | b > 0 = case a `quot` b of
            q -> go b (a - (q * b)) v (u - (q * v))
        | otherwise = (u, v, a)
{-# INLINE extGCD #-}

-- | Chinese Remainder Theorem
--
--  x = r0 (mod m0), x = r1 (mod m1) ==> x (mod (lcm m0 m1))
--
-- >>> crt (10, 20) (10, 30)
-- Just (10,60)
-- >>> crt (10, 20) (10, 20)
-- Just (10,20)
-- >>> crt (10, 20) (11, 20)
-- Nothing
crt :: (Integral a) => (a, a) -> (a, a) -> Maybe (a, a)
crt (!r0, !m0) (!r1, !m1)
    | mod (r1 - r0) g == 0 = Just $! (r, m)
    | otherwise = Nothing
  where
    -- m0 * p + m1 * q == g
    (p, q, g) = extGCD m0 m1
    !m = m0 * quot m1 g
    !r = mod (r0 + m0 * (quot (r1 - r0) g) * p) m


-- | * @p0@, @p1@ are prime
--   * @p0 /= p1@ or @r0 == r1@
--
-- >>> crt' (2,3) (3,5)
-- 8
-- >>> crt' (3,5) (3,7)
-- 3
-- >>> crt' (3,7) (3,7)
-- 3
crt' :: (Integral a) => (a, a) -> (a, a) -> a
crt' (r0, p0) (r1, p1)
    = mod (r0 + p0 * recipMod p0 p1 `rem` (p0 * p1) * (r1 - r0)) (p0 * p1)

-- |
-- >>> crts [(20,30),(30,50),(20,70)]
-- Just (230,1050)
-- >>> crts []
-- Just (0,1)
-- >>> crts [(1, 10), (2, 20), (3, 30)]
-- Nothing
crts :: (Integral a) => [(a, a)] -> Maybe (a, a)
crts cs = foldr ((>=>).crt) return cs $ (0, 1)

-- |
-- x (mod m) (x = r[i] (mod m[i]))
--
-- gcd m[i] m[j] == 1
--
-- /O(n^2)/
--
-- >>> garner [(2, 3), (3, 5), (2, 7)] 999
-- 23
-- >>> garner [(2, 3), (3, 5), (2, 7)] 10
-- 3
garner :: (Integral a) => [(a, a)] -> a -> a
garner rms m0 = go [] rms
  where
    ms = map snd rms
    go cs ((r,m):rest) = go (cs ++ [c]) rest
      where
        -- a + b * c = r (mod m)
        (a, b) = F.foldl' (step m) (0, 1) $ zip cs ms
        !c  = rem (mod (r - a) m * recipMod b m) m
    go cs [] = fst . F.foldl' (step m0) (0, 1) $ zip cs ms

    step m (!s, !p) (ci, mi) = (s', p')
      where
        !s' = rem (s + rem (ci * p) m) m
        !p' = rem (p * mi) m
