module Test.Prop.MonoidAction (monoidActionSpec) where

import Data.Monoid.Action (MonoidAction (..))
import Test.Prelude

monoidActionSpec ::
  forall f a.
  ( Arbitrary f
  , Monoid f
  , Show f
  , Arbitrary a
  , Show a
  , Eq a
  , MonoidAction f a
  ) =>
  Proxy f ->
  Proxy a ->
  Spec
monoidActionSpec _ _ = do
  describe "Monoid Action (monoid morphsm to Endo)" $ do
    prop "mact mempty = id" $ prop_appMempty @f @a (Proxy @f)
    prop "mact (f <> g) x == mact f (mact g x)"
      $ prop_appMappend @f @a

prop_appMempty ::
  forall f a.
  (MonoidAction f a, Eq a) =>
  Proxy f ->
  a ->
  Bool
prop_appMempty _ x = mact @f @a mempty x == x

prop_appMappend ::
  forall f a.
  (MonoidAction f a, Eq a) =>
  f ->
  f ->
  a ->
  Bool
prop_appMappend f g x = mact (f <> g) x == mact f (mact g x)
