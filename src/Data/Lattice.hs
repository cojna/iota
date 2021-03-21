{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Lattice where

import Control.Monad
import Control.Monad.Primitive
import Data.Bits
import Data.Coerce
import qualified Data.List as L
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Math.Prime

import My.Prelude (rep, rev)

infix 4 .<.
class Poset a where
  (.<.) :: a -> a -> Bool
  zeta :: (Integral i) => a -> a -> i
  zeta x y
    | x .<. y = 1
    | otherwise = 0
  moebius :: (Integral i) => a -> a -> i

class Lattice a where
  (/\) :: a -> a -> a
  (\/) :: a -> a -> a

class FastZetaMoebius f where
  type Dim f
  fastZeta ::
    (Num a, U.Unbox a, PrimMonad m) =>
    (Int -> f Int) ->
    Dim f ->
    UM.MVector (PrimState m) a ->
    m ()
  fastMoebius ::
    (Num a, U.Unbox a, PrimMonad m) =>
    (Int -> f Int) ->
    Dim f ->
    UM.MVector (PrimState m) a ->
    m ()

newtype NatOrd a = NatOrd {getNatOrd :: a}
  deriving (Eq, Ord, Show)

instance (Integral a, Ord a) => Poset (NatOrd a) where
  (.<.) = (<=)
  {-# INLINE (.<.) #-}
  moebius (NatOrd x) (NatOrd y)
    | x == y = 1
    | x + 1 == y = -1
    | otherwise = 0

instance (Ord a) => Lattice (NatOrd a) where
  (/\) = coerce (min @a)
  {-# INLINE (/\) #-}
  (\/) = coerce (max @a)
  {-# INLINE (\/) #-}

newtype DivOrd a = DivOrd {getDivOrd :: a}
  deriving (Eq, Show)

instance (Integral a) => Poset (DivOrd a) where
  (.<.) (DivOrd x) (DivOrd y) = rem y x == 0
  {-# INLINE (.<.) #-}
  moebius (DivOrd x) (DivOrd y)
    | not $ DivOrd x .<. DivOrd y = 0
    | otherwise =
      product . map mu . L.group $
        primeFactors (quot y x)
    where
      mu [_] = -1
      mu _ = 0

instance (Integral a) => Lattice (DivOrd a) where
  (/\) = coerce (gcd @a)
  {-# INLINE (/\) #-}
  (\/) = coerce (lcm @a)
  {-# INLINE (\/) #-}

instance FastZetaMoebius DivOrd where
  type Dim DivOrd = U.Vector Int
  fastZeta _ primes g = do
    let n = UM.length g
    when (n > 0) $ do
      g0 <- UM.read g 0
      U.forM_ primes $ \p ->
        rev (quot (n - 1) p + 1) $ \i -> do
          c <- UM.unsafeRead g (p * i)
          UM.unsafeModify g (+ c) i
      UM.write g 0 g0
  {-# INLINE fastZeta #-}

  fastMoebius _ primes f = do
    let n = UM.length f
    when (n > 0) $ do
      f0 <- UM.read f 0
      U.forM_ primes $ \p ->
        rep (quot (n - 1) p + 1) $ \i -> do
          c <- UM.unsafeRead f (p * i)
          UM.unsafeModify f (subtract c) i
      UM.write f 0 f0
  {-# INLINE fastMoebius #-}

newtype SetOrd a = SetOrd {getSetOrd :: a}
  deriving (Eq, Show)

instance (Bits a) => Poset (SetOrd a) where
  (.<.) (SetOrd x) (SetOrd y) = x .&. y == x
  {-# INLINE (.<.) #-}
  moebius (SetOrd x) (SetOrd y)
    | not $ SetOrd x .<. SetOrd y = 0
    | testBit (popCount $ complement x .&. y) 0 = -1
    | otherwise = 1

instance (Bits a) => Lattice (SetOrd a) where
  (/\) = coerce ((.&.) @a)
  {-# INLINE (/\) #-}
  (\/) = coerce ((.|.) @a)
  {-# INLINE (\/) #-}

instance FastZetaMoebius SetOrd where
  type Dim SetOrd = Int
  fastZeta _ n g = do
    rep n $ \i -> do
      rep (unsafeShiftL 1 n) $ \j -> do
        unless (testBit j i) $ do
          c <- UM.unsafeRead g (setBit j i)
          UM.unsafeModify g (+ c) j
  {-# INLINE fastZeta #-}
  fastMoebius _ n f = do
    rep n $ \i -> do
      rep (unsafeShiftL 1 n) $ \j -> do
        unless (testBit j i) $ do
          c <- UM.unsafeRead f (setBit j i)
          UM.unsafeModify f (subtract c) j
  {-# INLINE fastMoebius #-}
