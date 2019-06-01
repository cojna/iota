{-# LANGUAGE ViewPatterns #-}

module Util.MonoidProp where

import           Data.Monoid
import           Data.Proxy
import           Test.Hspec
import           Test.Hspec.QuickCheck     (prop)
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary


monoidSpec :: (Arbitrary a, Monoid a, Show a, Eq a) => Proxy a -> Spec
monoidSpec proxy = do
    describe "Monoid law" $ do
        prop "mempty <> x == x <> mempty == x" $ prop_unit proxy
        prop "(x <> y) <> z == x <> (y <> z)" $ prop_associative proxy

prop_unit :: (Monoid a, Eq a) => Proxy a -> a -> Bool
prop_unit _ x = mempty <> x == x && x <> mempty == x

prop_associative :: (Monoid a, Eq a) => Proxy a -> a -> a -> a -> Bool
prop_associative _ x y z = (x <> y) <> z == x <> (y <> z)
