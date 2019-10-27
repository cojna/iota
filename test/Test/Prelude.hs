{-# LANGUAGE DataKinds, KindSignatures, ScopedTypeVariables #-}

module Test.Prelude
    ( module Test.Hspec
    , module Test.Hspec.QuickCheck
    , module Test.QuickCheck
    , module Test.QuickCheck.Arbitrary
    , module Test.QuickCheck.Monadic
    , evaluate
    , withTLEmsec
    , Prime(..)
    , ByteStringOf(..)
    ) where

import           Control.Exception         (Exception (..), evaluate, throwIO)
import qualified Data.ByteString.Char8     as C
import           Data.Coerce
import           Data.IntMod
import           Data.Proxy
import           GHC.TypeLits
import           Math.Prime                (smallPrimes)
import           System.Timeout            (timeout)
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Monadic

data TimeLimitExceeded = TimeLimitExceeded deriving (Show)

instance Exception TimeLimitExceeded

withTLEmsec :: Int -> IO a -> IO a
withTLEmsec msec action = do
    res <- timeout (msec * 1000) action
    case res of
        Just x  -> return x
        Nothing -> throwIO TimeLimitExceeded

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
