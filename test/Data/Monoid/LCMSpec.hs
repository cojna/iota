{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Monoid.LCMSpec (main, spec) where

import Data.Coerce
import Data.Monoid.LCM
import Data.Proxy
import Test.Prelude
import Test.Prop.Monoid

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "LCM (Small Word)" $
    monoidSpec (Proxy :: Proxy (LCM (Small Word)))

instance Arbitrary a => Arbitrary (LCM a) where
  arbitrary = (coerce :: a -> LCM a) <$> arbitrary
