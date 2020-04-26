module Data.Matrix2x2 where

data Mat2x2 a = M22
    !a !a
    !a !a
    deriving Eq

instance Functor Mat2x2 where
    fmap f (M22 a b c d) = M22
        (f a) (f b)
        (f c) (f d)

instance (Show a) => Show (Mat2x2 a) where
    show (M22 a b c d) = unlines $ map show
        [ [a, b]
        , [c, d]
        ]
instance (Num a) => Num (Mat2x2 a) where
    (M22 a0 b0 c0 d0) + (M22 a1 b1 c1 d1)
        = M22
            (a0+a1) (b0+b1)
            (c0+c1) (d0+d1)
    (M22 a0 b0 c0 d0) - (M22 a1 b1 c1 d1)
        = M22
            (a0-a1) (b0-b1)
            (c0-c1) (d0-d1)
    (M22 a0 b0 c0 d0) * (M22 a1 b1 c1 d1)
        = M22
            (a0*a1+b0*c1) (a0*b1+b0*d1)
            (c0*a1+d0*c1) (c0*b1+d0*d1)
    negate = fmap negate
    abs = id
    signum = const 1
    fromInteger x = M22
        (fromInteger x) 0
        0 (fromInteger x)

tr :: (Num a) => Mat2x2 a -> a
tr (M22 a _ c _) = a + c

det :: (Num a) => Mat2x2 a -> a
det (M22 a b c d) = a * d - b * c

unsafeInverse :: (Fractional a) => Mat2x2 a -> Mat2x2 a
unsafeInverse mat@(M22 a b c d) = (recip (det mat)*) <$> M22 d (-b) (-c) a
