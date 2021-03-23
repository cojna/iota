{-# OPTIONS_GHC -Wno-orphans #-}

module Geometry.Instances where

import Geometry
import Geometry.Circle
import Test.Prelude

instance (Arbitrary a) => Arbitrary (Point a) where
  arbitrary = P <$> arbitrary <*> arbitrary

instance (Arbitrary a) => Arbitrary (Circle a) where
  arbitrary = Circle <$> arbitrary <*> arbitrary

instance (Floating a, Ord a) => Approx (Circle a) where
  approx eps (Circle c0 r0) (Circle c1 r1) =
    sqrNorm (c1 - c0) <= realToFrac eps
      && abs (r1 - r0) <= realToFrac eps
