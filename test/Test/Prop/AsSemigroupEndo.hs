module Test.Prop.AsSemigroupEndo (asSemigroupEndoSpec) where

import Data.SegTree (AsSemigroupEndo (..))
import Test.Prelude

asSemigroupEndoSpec ::
  forall f a.
  ( Arbitrary f
  , Monoid f
  , Show f
  , Arbitrary a
  , Semigroup a
  , Show a
  , Eq a
  , AsSemigroupEndo f a
  ) =>
  Proxy f ->
  Proxy a ->
  Spec
asSemigroupEndoSpec _ _ = do
  describe "Act as Semigroup Endomorphism" $ do
    prop "sendo mempty = id" $ prop_appMempty @f @a (Proxy @f)
    prop "sendo (f <> g) x == sendo f (sendo g x)"
      $ prop_appMappend @f @a
    prop "sendo f (x <> y) = sendo f x <> sendo f y"
      $ prop_semigroupEndomorphism @f @a

prop_appMempty ::
  forall f a.
  (AsSemigroupEndo f a, Eq a) =>
  Proxy f ->
  a ->
  Bool
prop_appMempty _ x = sendo @f @a mempty x == x

prop_appMappend ::
  forall f a.
  (AsSemigroupEndo f a, Eq a) =>
  f ->
  f ->
  a ->
  Bool
prop_appMappend f g x = sendo (f <> g) x == sendo f (sendo g x)

prop_semigroupEndomorphism ::
  (AsSemigroupEndo f a, Semigroup a, Eq a) => f -> a -> a -> Bool
prop_semigroupEndomorphism f x y =
  sendo f (x <> y) == sendo f x <> sendo f y
