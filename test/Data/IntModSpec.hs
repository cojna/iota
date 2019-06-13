{-# LANGUAGE ViewPatterns #-}

module Data.IntModSpec where

import           Data.Coerce
import           Data.IntMod
import           Data.Proxy
import           Test.Hspec
import           Test.Hspec.QuickCheck     (prop)
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary
import           Test.Prop.Num
import           Test.Prop.Integral
import           Test.Prop.Fractional

instance Arbitrary IntMod where
    arbitrary = coerce . intMod <$> (arbitrary :: Gen Int)

spec :: Spec
spec = do
    numSpec (Proxy :: Proxy IntMod)
    numClosedSpec intModValidate
    integralSpec (Proxy :: Proxy IntMod)
    fractionalSpec (Proxy :: Proxy IntMod)
    fractionalClosedSpec intModValidate
