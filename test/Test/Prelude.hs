module Test.Prelude
    ( module Test.Hspec
    , module Test.Hspec.QuickCheck
    , module Test.QuickCheck
    , module Test.QuickCheck.Arbitrary
    , Prime(..)
    ) where

import           Data.Coerce
import           Data.IntMod
import           Math.Prime (smallPrimes)
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
