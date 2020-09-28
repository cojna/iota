{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}

module Data.SegTreeSpec (main, spec) where

import           Data.Bits
import           Data.IntMod
import           Data.Monoid    hiding (First (..), Last (..))
import           Data.SegTree
import           Data.Semigroup
import           Test.Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "MonoidAction () (Product IntMod)" $ do
        monoidActionSpec (Proxy @()) (Proxy @(Product IntMod))
    describe "MonoidAction (Sum Int) (Min Int)" $ do
        monoidActionSpec (Proxy @(Sum Int)) (Proxy @(Min Int))
    describe "MonoidAction (Sum Int) (Max Int)" $ do
        monoidActionSpec (Proxy @(Sum Int)) (Proxy @(Max Int))
    describe "MonoidAction (Sum Int) (Sum Int, Sum Int)" $ do
        monoidActionSpec (Proxy @(Sum Int)) (Proxy @(Sum Int, Sum Int))
    describe "MonoidAction (Product Int) (Sum Int)" $ do
        monoidActionSpec (Proxy @(Product Int)) (Proxy @(Sum Int))
    describe "MonoidAction (Dual (Maybe (Last (Min Int)))) (Min Int)" $ do
        monoidActionSpec (Proxy @(Dual (Maybe (Last (Min Int))))) (Proxy @(Min Int))
    describe "MonoidAction (Dual (Maybe (Last (Max Int)))) (Max Int)" $ do
        monoidActionSpec (Proxy @(Dual (Maybe (Last (Max Int))))) (Proxy @(Max Int))
    describe "MonoidAction (Min Int) (Min Int)" $ do
        monoidActionSpec (Proxy @(Min Int)) (Proxy @(Min Int))
    describe "MonoidAction (Max Int) (Max Int)" $ do
        monoidActionSpec (Proxy @(Max Int)) (Proxy @(Max Int))

monoidActionSpec :: forall f a.
    ( Arbitrary f, Monoid f, Show f
    , Arbitrary a, Monoid a, Show a, Eq a
    , MonoidAction f a
    )
    => Proxy f -> Proxy a -> Spec
monoidActionSpec _ _ = do
    describe "Monoid Action on Semigroup" $ do
        prop "appMonoid mempty = id" $ prop_appMempty @f @a (Proxy @f)
        prop "appMonoid (f <> g) x == appMonoid f (appMonoid g x)"
            $ prop_appMappend @f @a
        prop "appMonoid f (x <> y) = appMonoid f x <> appMonoid f y"
            $ prop_monoidHomomorphismMappend @f @a

prop_appMempty :: forall f a.(MonoidAction f a, Eq a)
    => Proxy f -> a -> Bool
prop_appMempty _ x = appMonoid @f @a mempty x == x

prop_appMappend :: forall f a.(MonoidAction f a, Eq a)
    => f -> f -> a -> Bool
prop_appMappend f g x = appMonoid (f <> g) x == appMonoid f (appMonoid g x)

prop_monoidHomomorphismMappend
    :: (MonoidAction f a, Monoid a, Eq a) => f -> a -> a -> Bool
prop_monoidHomomorphismMappend f x y
    = appMonoid f (x <> y) == appMonoid f x <> appMonoid f y
