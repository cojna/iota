{-# LANGUAGE ViewPatterns #-}

module Data.IntModSpec (main, spec) where

import           Data.Coerce
import           Data.IntMod
import           Data.Proxy
import           Test.Prelude
import           Test.Prop.Num
import           Test.Prop.Integral
import           Test.Prop.Fractional

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    numSpec (Proxy :: Proxy IntMod)
    numClosedSpec intModValidate
    integralSpec (Proxy :: Proxy IntMod)
    fractionalSpec (Proxy :: Proxy IntMod)
    fractionalClosedSpec intModValidate
