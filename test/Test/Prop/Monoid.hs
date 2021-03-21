{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Prop.Monoid where

import Data.Monoid
import Test.Prelude

monoidSpec ::
  forall a. (Arbitrary a, Monoid a, Show a, Eq a) => Proxy a -> Spec
monoidSpec _ = do
  describe "Monoid law" $ do
    prop "mempty <> x == x <> mempty == x" $ prop_unit @a
    prop "(x <> y) <> z == x <> (y <> z)" $ prop_associative @a

{-# ANN prop_unit "HLint: ignore Monoid law, left identity" #-}
{-# ANN prop_unit "HLint: ignore Monoid law, right identity" #-}
prop_unit :: (Monoid a, Eq a) => a -> Bool
prop_unit x = mempty <> x == x && x <> mempty == x

prop_associative :: (Monoid a, Eq a) => a -> a -> a -> Bool
prop_associative x y z = (x <> y) <> z == x <> (y <> z)
