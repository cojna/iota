{-# LANGUAGE DataKinds, KindSignatures, ScopedTypeVariables #-}

module Test.Prelude
    ( module Test.Hspec
    , module Test.Hspec.QuickCheck
    , module Test.QuickCheck
    , module Test.QuickCheck.Arbitrary
    , evaluate
    , Prime(..)
    , ByteStringOf(..)
    ) where

import           Control.Exception         (evaluate)
import qualified Data.ByteString.Char8     as C
import           Data.Coerce
import           Data.IntMod
import           Data.Proxy
import           GHC.TypeLits
import           Math.Prime                (smallPrimes)
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary

newtype Prime a = Prime {getPrime :: a}
    deriving (Eq, Ord, Show)

instance (Integral a) => Arbitrary (Prime a) where
    arbitrary = Prime . fromIntegral <$> elements smallPrimes

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
