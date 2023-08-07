module Math.SternBrocot where

sternBrocot :: (Frac -> Ordering) -> Frac
sternBrocot approx = go (0 :/ 1) (1 :/ 0)
  where
    go l@(lp :/ lq) r@(rp :/ rq) = case approx m of
      LT -> go m r
      GT -> go l m
      EQ -> m
      where
        !m = (lp + rp) :/ (lq + rq)

data Frac = !Int :/ !Int deriving (Eq)

instance Show Frac where
  show (x :/ y) = shows x $ '/' : show y

instance Ord Frac where
  compare (x0 :/ y0) (x1 :/ y1) = compare (x0 * y1) (x1 * y0)

frac :: Int -> Int -> Frac
frac x y = case gcd x y of
  g -> (signum y * quot x g) :/ quot (abs y) g

reduceFrac :: Int -> Int -> Frac
reduceFrac x y = case gcd x y of
  g -> quot x g :/ quot y g

instance Num Frac where
  (x0 :/ y0) + (x1 :/ y1) = reduceFrac (x0 * y1 + x1 * y0) (y0 * y1)
  (x0 :/ y0) - (x1 :/ y1) = reduceFrac (x0 * y1 - x1 * y0) (y0 * y1)
  (x0 :/ y0) * (x1 :/ y1) = reduceFrac (x0 * x1) (y0 * y1)
  negate (x :/ y) = negate x :/ y
  abs (x :/ y) = abs x :/ y
  signum (x :/ _) = signum x :/ 1
  fromInteger = (:/ 1) . fromInteger
