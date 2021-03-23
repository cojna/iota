{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Mat3x3Spec (main, spec) where

import Control.Monad
import Data.IntMod
import Data.Mat3x3
import Data.Primitive
import Test.Prelude
import Test.Prop.Num

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  numSpec (Proxy :: Proxy (Mat3x3 IntMod))

instance (Arbitrary a, Prim a) => Arbitrary (Mat3x3 a) where
  arbitrary = fromList <$> replicateM 9 arbitrary
