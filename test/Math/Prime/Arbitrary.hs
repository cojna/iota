module Math.Prime.Arbitrary where

import           Math.Prime                (smallPrimes)
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary

newtype Prime a = Prime {getPrime :: a}
    deriving (Eq, Ord, Show)

instance (Integral a) => Arbitrary (Prime a) where
    arbitrary = Prime . fromIntegral <$> elements smallPrimes
