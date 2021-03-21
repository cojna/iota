module Data.IntMod.SafeSpec (main, spec) where

import Data.Coerce
import Data.Int
import Data.IntMod.Safe
import Data.Proxy
import Test.Prelude
import Test.Prop.Fractional
import Test.Prop.Integral
import Test.Prop.Num

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  numSpec (Proxy :: Proxy IntMod)
  numClosedSpec intModValidate
  integralSpec (Proxy :: Proxy IntMod)
  fractionalSpec (Proxy :: Proxy IntMod)
  fractionalClosedSpec intModValidate

instance Arbitrary IntMod where
  arbitrary = coerce . intMod <$> (arbitrary :: Gen Int64)
