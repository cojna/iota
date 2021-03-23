{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.LatticeSpec (main, spec) where

import Data.Lattice
import qualified Data.Vector.Unboxed as U
import Math.Prime.Sieve (withPrimes)
import Test.Prelude

#define MAX_PRIME 46337

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Poset" $ do
    describe "NatOrd" $ posetSpec (Proxy @(NatOrd Int))
    describe "SetOrd" $ posetSpec (Proxy @(SetOrd Int))
    describe "DivOrd" $ posetSpec (Proxy @(DivOrd Int))
  describe "Lattice" $ do
    describe "NatOrd" $ latticeSpec (Proxy @(NatOrd Int))
    describe "SetOrd" $ latticeSpec (Proxy @(SetOrd Int))
    describe "DivOrd" $ latticeSpec (Proxy @(DivOrd Int))
  describe "fast zeta/moebius transform" $ do
    describe "SetOrd" $
      withNatProxy @5 $ \natProxy -> do
        prop "same to naive zeta" $ prop_naiveZetaSetOrd natProxy
        prop "same to naive moebius" $ prop_naiveMoebiusSetOrd natProxy
        prop "moebius . zeta = id" $ prop_zetaMoebiusSetOrd natProxy
        prop "zeta . moebius = id" $ prop_moebiusZetaSetOrd natProxy
    describe "DivOrd" $
      withPrimes MAX_PRIME $ \primes -> do
        prop "same to naive zeta" $ prop_naiveZetaDivOrd primes
        prop "same to naive moebius" $ prop_naiveMoebiusDivOrd primes
        prop "moebius . zeta = id" $ prop_zetaMoebiusDivOrd primes
        prop "zeta . moebius = id" $ prop_moebiusZetaDivOrd primes

instance (Arbitrary a) => Arbitrary (NatOrd a) where
  arbitrary = NatOrd <$> arbitrary @a

instance (Arbitrary a) => Arbitrary (SetOrd a) where
  arbitrary = SetOrd <$> arbitrary @a

instance (Num a, Ord a, Arbitrary a) => Arbitrary (DivOrd a) where
  arbitrary = DivOrd . getPositive <$> arbitrary @(Positive a)

posetSpec ::
  forall a.
  (Poset a, Eq a, Show a, Arbitrary a) =>
  Proxy a ->
  Spec
posetSpec _ = do
  prop "x .<. x" $ prop_reflexivity @a
  prop "x .<. y && y .<. x ==> x == y" $ prop_antisymmmetry @a
  prop "x .<. y && y .<. z ==> x .<. z" $ prop_transitivity @a

prop_reflexivity :: (Poset a) => a -> Bool
prop_reflexivity x = x .<. x

prop_antisymmmetry :: (Poset a, Eq a) => a -> a -> Bool
prop_antisymmmetry x y = not (x .<. y && y .<. x) || x == y

prop_transitivity :: (Poset a) => a -> a -> a -> Bool
prop_transitivity x y z = not (x .<. y && y .<. z) || x .<. z

latticeSpec ::
  forall a.
  (Lattice a, Eq a, Show a, Arbitrary a) =>
  Proxy a ->
  Spec
latticeSpec _ = do
  describe "commutative" $ do
    prop "x \\/ y == y \\/ x" $ prop_joinCommutative @a
    prop "x /\\ == y /\\ x" $ prop_meetCommutative @a
  describe "associative" $ do
    prop "x \\/ (y \\/ z) == (x \\/ y) \\/ z" $ prop_joinAssociative @a
    prop "x /\\ (y /\\ z) == (x /\\ y) /\\ z" $ prop_meetAssociative @a
  describe "absorption" $ do
    prop "x \\/ (x /\\ y) == x" $ prop_joinAbsorption @a
    prop "x /\\ (x \\/ y) == x" $ prop_meetAbsorption @a

prop_joinCommutative :: (Lattice a, Eq a) => a -> a -> Bool
prop_joinCommutative x y = x \/ y == y \/ x

prop_meetCommutative :: (Lattice a, Eq a) => a -> a -> Bool
prop_meetCommutative x y = x /\ y == y /\ x

prop_joinAssociative :: (Lattice a, Eq a) => a -> a -> a -> Bool
prop_joinAssociative x y z = x \/ (y \/ z) == (x \/ y) \/ z

prop_meetAssociative :: (Lattice a, Eq a) => a -> a -> a -> Bool
prop_meetAssociative x y z = x /\ (y /\ z) == (x /\ y) /\ z

prop_joinAbsorption :: (Lattice a, Eq a) => a -> a -> Bool
prop_joinAbsorption x y = x \/ (x /\ y) == x

prop_meetAbsorption :: (Lattice a, Eq a) => a -> a -> Bool
prop_meetAbsorption x y = x /\ (x \/ y) == x

withNatProxy :: forall n a. (Proxy (n :: Nat) -> a) -> a
withNatProxy = ($ (Proxy @n))

naiveZeta :: (Poset (f Int)) => (Int -> f Int) -> U.Vector Int -> U.Vector Int
naiveZeta f xs = U.generate n $ \v ->
  sum [xs U.! u | u <- [0 .. n -1], f v .<. f u]
  where
    n = U.length xs

naiveMoebius :: (Poset (f Int)) => (Int -> f Int) -> U.Vector Int -> U.Vector Int
naiveMoebius f xs = U.generate n $ \v ->
  sum [moebius (f v) (f u) * xs U.! u | u <- [0 .. n -1]]
  where
    n = U.length xs

prop_naiveZetaSetOrd ::
  (KnownNat n) => Proxy n -> SizeFixedList (2 ^ n) Int -> Bool
prop_naiveZetaSetOrd
  (fromIntegral . natVal -> n)
  (U.fromList . getSizeFixedList -> xs) =
    U.modify (fastZeta SetOrd n) xs == naiveZeta SetOrd xs

prop_naiveMoebiusSetOrd ::
  (KnownNat n) => Proxy n -> SizeFixedList (2 ^ n) Int -> Bool
prop_naiveMoebiusSetOrd
  (fromIntegral . natVal -> n)
  (U.fromList . getSizeFixedList -> xs) =
    U.modify (fastMoebius SetOrd n) xs == naiveMoebius SetOrd xs

prop_zetaMoebiusSetOrd ::
  (KnownNat n) => Proxy n -> SizeFixedList (2 ^ n) Int -> Bool
prop_zetaMoebiusSetOrd
  (fromIntegral . natVal -> n)
  (U.fromList . getSizeFixedList -> xs) =
    xs == transformed
    where
      transformed =
        U.modify
          ( \mf -> do
              fastMoebius SetOrd n mf
              fastZeta SetOrd n mf
          )
          xs

prop_moebiusZetaSetOrd ::
  (KnownNat n) => Proxy n -> SizeFixedList (2 ^ n) Int -> Bool
prop_moebiusZetaSetOrd
  (fromIntegral . natVal -> n)
  (U.fromList . getSizeFixedList -> xs) =
    xs == transformed
    where
      transformed =
        U.modify
          ( \mf -> do
              fastZeta SetOrd n mf
              fastMoebius SetOrd n mf
          )
          xs

type Primes = U.Vector Int

naiveZetaDivOrd :: U.Vector Int -> U.Vector Int
naiveZetaDivOrd xs = U.generate n $ \v ->
  if v > 0
    then sum [xs U.! u | u <- [1 .. n -1], DivOrd v .<. DivOrd u]
    else xs U.! 0
  where
    n = U.length xs

naiveMoebiusDivOrd :: U.Vector Int -> U.Vector Int
naiveMoebiusDivOrd xs = U.generate n $ \v ->
  if v > 0
    then sum [moebius (DivOrd v) (DivOrd u) * xs U.! u | u <- [1 .. n -1]]
    else xs U.! 0
  where
    n = U.length xs

prop_naiveZetaDivOrd :: Primes -> [Int] -> Bool
prop_naiveZetaDivOrd primes (U.fromList -> xs) =
  U.modify (fastZeta DivOrd primes) xs == naiveZetaDivOrd xs

prop_naiveMoebiusDivOrd :: Primes -> [Int] -> Bool
prop_naiveMoebiusDivOrd primes (U.fromList -> xs) =
  U.modify (fastMoebius DivOrd primes) xs == naiveMoebiusDivOrd xs

prop_zetaMoebiusDivOrd :: Primes -> [Int] -> Bool
prop_zetaMoebiusDivOrd primes xs = xs == transformed
  where
    transformed =
      U.toList
        . U.modify
          ( \mf -> do
              fastMoebius DivOrd primes mf
              fastZeta DivOrd primes mf
          )
        $ U.fromList xs

prop_moebiusZetaDivOrd :: Primes -> [Int] -> Bool
prop_moebiusZetaDivOrd primes xs = xs == transformed
  where
    transformed =
      U.toList
        . U.modify
          ( \mf -> do
              fastZeta DivOrd primes mf
              fastMoebius DivOrd primes mf
          )
        $ U.fromList xs
