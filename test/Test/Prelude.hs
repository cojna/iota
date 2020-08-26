{-# LANGUAGE BangPatterns, DataKinds, KindSignatures, ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Prelude
    ( module GHC.TypeLits
    , module Data.Proxy
    , module Test.Hspec
    , module Test.Hspec.QuickCheck
    , module Test.QuickCheck
    , module Test.QuickCheck.Arbitrary
    , module Test.QuickCheck.Monadic
    , evaluate
    , withTLEmsec
    , Prime(..)
    , Modulo(..)
    , ByteStringOf(..)
    , SizeFixedList(..)
    , SizeBoundedList(..)
    , Applox(..)
    ) where

import           Control.Exception         (Exception (..), evaluate, throwIO)
import qualified Data.ByteString.Char8     as C
import           Data.Coerce
import           Data.EPS
import           Data.IntMod
import           Data.Proxy
import           Data.Semigroup
import           GHC.TypeLits
import           Math.Prime                (smallPrimes)
import           System.Timeout            (timeout)
import           Test.Hspec                hiding (Arg)
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Monadic

data TimeLimitExceeded = TimeLimitExceeded Int

instance Show TimeLimitExceeded where
    show (TimeLimitExceeded msec)
        = "TimeLimitExceeded (" <> shows msec " msec)"

instance Exception TimeLimitExceeded

withTLEmsec :: Int -> IO a -> IO a
withTLEmsec msec action = do
    res <- timeout (msec * 1000) action
    case res of
        Just x  -> return x
        Nothing -> throwIO (TimeLimitExceeded msec)

newtype Prime a = Prime {getPrime :: a}
    deriving (Eq, Ord, Show)

instance (Integral a) => Arbitrary (Prime a) where
    arbitrary = Prime . fromIntegral <$> elements smallPrimes

newtype Modulo (n :: Nat) a = Moddulo {getModulo :: a}
    deriving (Eq, Ord, Show)

instance (Integral a, Arbitrary a, KnownNat n) => Arbitrary (Modulo n a) where
    arbitrary = coerce . flip mod m <$> (arbitrary :: Gen a)
      where
        !m = fromIntegral $ natVal (Proxy :: Proxy n)

instance Arbitrary IntMod where
    arbitrary = coerce . intMod <$> (arbitrary :: Gen Int)

newtype ByteStringOf (s :: Symbol)
    = ByteStringOf { getByteStringOf :: C.ByteString }

instance Show (ByteStringOf s) where
    show = show . getByteStringOf

instance KnownSymbol s => Arbitrary (ByteStringOf s) where
    arbitrary = coerce
        . fmap C.pack
        . listOf
        . elements
        $ symbolVal (Proxy :: Proxy s)

newtype SizeFixedList (n :: Nat) a = SizeFixedList{getSizeFixedList :: [a]}
    deriving (Eq, Ord, Show)

instance (Arbitrary a, KnownNat n) => Arbitrary (SizeFixedList n a) where
    arbitrary = SizeFixedList <$> vectorOf
        (fromIntegral $ natVal (Proxy @n))
        (arbitrary @a)

newtype SizeBoundedList (n :: Nat) a = SizeBoundedList{ getSizeBoundedList :: [a]}
    deriving (Eq, Ord, Show)

instance (Arbitrary a, KnownNat n) => Arbitrary (SizeBoundedList n a) where
    arbitrary = SizeBoundedList . take (fromIntegral $ natVal (Proxy @n))
        <$> arbitrary @[a]

instance (Arbitrary a) => Arbitrary (EPS a) where
    arbitrary = EPS <$> arbitrary

class Applox a where
    approx :: Double -> a -> a -> Bool

instance Applox Double where
    approx e ans x = absErr ans x < e || relErr ans x < e

instance (Applox a, Applox b) => Applox (a, b) where
    approx e (x0, y0) (x, y) = approx e x0 x && approx e y0 y

instance (Applox a, Applox b, Applox c) => Applox (a, b, c) where
    approx e (x0, y0, z0) (x, y, z)
        = approx e x0 x && approx e y0 y && approx e z0 z

instance (Applox a, Applox b) => Applox (Arg a b) where
    approx e (Arg v0 k0) (Arg v k) = approx e v0 v && approx e k0 k
