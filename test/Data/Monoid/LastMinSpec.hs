{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Monoid.LastMinSpec (main, spec) where

import Data.Coerce
import Data.Monoid.LastMin
import Data.Proxy
import Test.Prelude
import Test.Prop.Monoid

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "LastMin Int" $
    monoidSpec (Proxy :: Proxy (LastMin Int))

instance Arbitrary a => Arbitrary (LastMin a) where
  arbitrary = (coerce :: a -> LastMin a) <$> arbitrary
