module Math.Linear.Vector3 where

data Vec3 a = V3 !a !a !a deriving (Eq, Ord)

instance Show a => Show (Vec3 a) where
    show (V3 x y z) = show [x, y, z]

instance Functor Vec3 where
    fmap f (V3 x y z) = V3 (f x) (f y) (f z)

instance (Num a) => Num (Vec3 a) where
    (V3 x0 y0 z0) + (V3 x1 y1 z1) = V3 (x0 + x1) (y0 + y1) (z0 + z1)
    (V3 x0 y0 z0) - (V3 x1 y1 z1) = V3 (x0 - x1) (y0 - y1) (z0 - z1)
    (*) = undefined
    negate = fmap negate
    abs = undefined
    signum = undefined
    fromInteger n = V3 (fromInteger n) 0 0

infixr 7 *:
(*:) :: Num a => a -> Vec3 a -> Vec3 a
(*:) k = fmap (k*)
{-# INLINE (*:) #-}

dot :: Num a => Vec3 a -> Vec3 a -> a
dot (V3 x0 y0 z0) (V3 x1 y1 z1) = x0 * x1 + y0 * y1 + z0 * z1
{-# INLINE dot #-}

cross :: Num a => Vec3 a -> Vec3 a -> Vec3 a
cross (V3 x0 y0 z0) (V3 x1 y1 z1) = V3
    (y0 * z1 - z0 * y1)
    (z0 * x1 - x0 * z1)
    (x0 * y1 - y0 * x1)
{-# INLINE cross #-}

norm1 :: Num a => Vec3 a -> a
norm1 (V3 x y z) = abs x + abs y + abs z
{-# INLINE norm1 #-}

norm2 :: Floating a => Vec3 a -> a
norm2 = sqrt . sqrNorm2
{-# INLINE norm2 #-}

sqrNorm2 :: Num a => Vec3 a -> a
sqrNorm2 v = v `dot` v
{-# INLINE sqrNorm2 #-}

normSup :: (Num a, Ord a) => Vec3 a -> a
normSup (V3 x y z) = abs x `max` abs y `max` abs z
{-# INLINE normSup #-}

normalize :: Floating a => Vec3 a -> Vec3 a
normalize v = recip (norm2 v) *: v
{-# INLINE normalize #-}
