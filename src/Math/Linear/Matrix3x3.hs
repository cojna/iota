module Math.Linear.Matrix3x3 where

data Mat3x3 a = M33
    !a !a !a
    !a !a !a
    !a !a !a
    deriving Eq

instance Functor Mat3x3 where
    fmap f (M33 a b c d e ff g h i) = M33
        (f a) (f b) (f c)
        (f d) (f e) (f ff)
        (f g) (f h) (f i)

instance (Show a) => Show (Mat3x3 a) where
    show (M33 a b c d e f g h i) = unlines $ map show
        [ [a, b, c]
        , [d, e, f]
        , [g, h, i]
        ]

instance (Num a) => Num (Mat3x3 a) where
    (M33 a0 b0 c0 d0 e0 f0 g0 h0 i0) + (M33 a1 b1 c1 d1 e1 f1 g1 h1 i1)
        = M33
            (a0+a1) (b0+b1) (c0+c1)
            (d0+d1) (e0+e1) (f0+f1)
            (g0+g1) (h0+h1) (i0+i1)
    (M33 a0 b0 c0 d0 e0 f0 g0 h0 i0) - (M33 a1 b1 c1 d1 e1 f1 g1 h1 i1)
        = M33
            (a0-a1) (b0-b1) (c0-c1)
            (d0-d1) (e0-e1) (f0-f1)
            (g0-g1) (h0-h1) (i0-i1)
    (M33 a0 b0 c0 d0 e0 f0 g0 h0 i0) * (M33 a1 b1 c1 d1 e1 f1 g1 h1 i1)
        = M33
            (a0*a1+b0*d1+c0*g1) (a0*b1+b0*e1+c0*h1) (a0*c1+b0*f1+c0*i1)
            (d0*a1+e0*d1+f0*g1) (d0*b1+e0*e1+f0*h1) (d0*c1+e0*f1+f0*i1)
            (g0*a1+h0*d1+i0*g1) (g0*b1+h0*e1+i0*h1) (g0*c1+h0*f1+i0*i1)
    negate = fmap negate
    abs = id
    signum = const 1
    fromInteger x = M33
        (fromInteger x) 0 0
        0 (fromInteger x) 0
        0 0 (fromInteger x)

tr :: (Num a) => Mat3x3 a -> a
tr (M33 a _ _ _ e _ _ _ i) = a + e + i

det :: (Num a) => Mat3x3 a -> a
det (M33 a b c d e f g h i) = a*e*i + b*f*g + c*d*h - c*e*g - b*d*i - a*f*h

unsafeInverse :: (Fractional a) => Mat3x3 a -> Mat3x3 a
unsafeInverse mat@(M33 a b c d e f g h i) = (recip (det mat) *) <$> mat'
  where
    mat' = M33
        (e*i-f*h) (c*h-b*i) (b*f-c*e)
        (f*g-d*i) (a*i-c*g) (c*d-a*f)
        (d*h-e*g) (b*g-a*h) (a*e-b*d)
