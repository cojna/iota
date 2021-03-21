module Data.IntModSpec (main, spec) where

import Data.Coerce
import Data.IntMod
import Data.Proxy
import Test.Prelude
import Test.Prop.Fractional
import Test.Prop.Integral
import Test.Prop.Num

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "(+%)" $ do
    prop "naive" prop_naiveAdd
  describe "(-%)" $ do
    prop "naive" prop_naiveSub
  describe "(*%)" $ do
    prop "naive" prop_naiveTimes
  numSpec (Proxy :: Proxy IntMod)
  numClosedSpec intModValidate
  integralSpec (Proxy :: Proxy IntMod)
  fractionalSpec (Proxy :: Proxy IntMod)
  fractionalClosedSpec intModValidate

prop_naiveAdd :: IntMod -> IntMod -> Bool
prop_naiveAdd (IntMod x) (IntMod y) =
  x +% y == mod (x + y) modulus

prop_naiveSub :: IntMod -> IntMod -> Bool
prop_naiveSub (IntMod x) (IntMod y) =
  x -% y == mod (x - y) modulus

prop_naiveTimes :: IntMod -> IntMod -> Bool
prop_naiveTimes (IntMod x) (IntMod y) =
  x *% y == mod (x * y) modulus
