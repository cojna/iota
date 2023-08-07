{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Monoid.LastMaxSpec (main, spec) where

import Data.Coerce
import Data.Monoid.LastMax
import Data.Proxy
import Test.Prelude
import Test.Prop.Monoid

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "LastMax Int" $
    monoidSpec (Proxy :: Proxy (LastMax Int))

instance (Arbitrary a) => Arbitrary (LastMax a) where
  arbitrary = (coerce :: a -> LastMax a) <$> arbitrary
