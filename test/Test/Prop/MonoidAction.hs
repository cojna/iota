{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Prop.MonoidAction (monoidActionSpec) where

import Data.SegTree (MonoidAction (..))
import Test.Prelude

monoidActionSpec ::
  forall f a.
  ( Arbitrary f
  , Monoid f
  , Show f
  , Arbitrary a
  , Monoid a
  , Show a
  , Eq a
  , MonoidAction f a
  ) =>
  Proxy f ->
  Proxy a ->
  Spec
monoidActionSpec _ _ = do
  describe "Monoid Action on Semigroup" $ do
    prop "appMonoid mempty = id" $ prop_appMempty @f @a (Proxy @f)
    prop "appMonoid (f <> g) x == appMonoid f (appMonoid g x)" $
      prop_appMappend @f @a
    prop "appMonoid f (x <> y) = appMonoid f x <> appMonoid f y" $
      prop_monoidHomomorphismMappend @f @a

prop_appMempty ::
  forall f a.
  (MonoidAction f a, Eq a) =>
  Proxy f ->
  a ->
  Bool
prop_appMempty _ x = appMonoid @f @a mempty x == x

prop_appMappend ::
  forall f a.
  (MonoidAction f a, Eq a) =>
  f ->
  f ->
  a ->
  Bool
prop_appMappend f g x = appMonoid (f <> g) x == appMonoid f (appMonoid g x)

prop_monoidHomomorphismMappend ::
  (MonoidAction f a, Monoid a, Eq a) => f -> a -> a -> Bool
prop_monoidHomomorphismMappend f x y =
  appMonoid f (x <> y) == appMonoid f x <> appMonoid f y
