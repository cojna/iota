{-# LANGUAGE BangPatterns #-}

module Data.Vector2 where

data Vec2 a = V2 !a !a deriving (Eq, Ord)

instance Show a => Show (Vec2 a) where
    show (V2 x y) = show [x, y]

instance Functor Vec2 where
    fmap f (V2 x y) = V2 (f x) (f y)

instance (Num a) => Num (Vec2 a) where
    (V2 x0 y0) + (V2 x1 y1) = V2 (x0 + x1) (y0 + y1)
    (V2 x0 y0) - (V2 x1 y1) = V2 (x0 - x1) (y0 - y1)
    (V2 x0 y0) * (V2 x1 y1) = V2 (x0 * x1 - y0 * y1) (x0 * y1 + x1 * y0)
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger n = V2 (fromInteger n) 0

instance (Fractional a) => Fractional (Vec2 a) where
    v0@(V2 x0 y0) / v1@(V2 x1 y1) = recip rr *: V2 x y
      where
        !rr = sqrNorm2 v1
        !x = v0 `dot` v1
        !y = v1 `cross` v0
    fromRational q = V2 (fromRational q) 0

infixr 7 *:
(*:) :: Num a => a -> Vec2 a -> Vec2 a
(*:) k = fmap (k*)
{-# INLINE (*:) #-}

dot :: Num a => Vec2 a -> Vec2 a -> a
dot (V2 x0 y0) (V2 x1 y1) = x0 * x1 + y0 * y1
{-# INLINE dot #-}

cross :: Num a => Vec2 a -> Vec2 a -> a
cross (V2 x0 y0) (V2 x1 y1) = x0 * y1 - y0 * x1
{-# INLINE cross #-}

norm1 :: Num a => Vec2 a -> a
norm1 (V2 x y) = abs x + abs y
{-# INLINE norm1 #-}

norm2 :: Floating a => Vec2 a -> a
norm2 = sqrt . sqrNorm2
{-# INLINE norm2 #-}

sqrNorm2 :: Num a => Vec2 a -> a
sqrNorm2 v = v `dot` v
{-# INLINE sqrNorm2 #-}

normSup :: (Num a, Ord a) => Vec2 a -> a
normSup (V2 x y) = abs x `max` abs y
{-# INLINE normSup #-}

normalize :: Floating a => Vec2 a -> Vec2 a
normalize v = recip (norm2 v) *: v
{-# INLINE normalize #-}
