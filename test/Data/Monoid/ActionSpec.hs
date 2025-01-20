module Data.Monoid.ActionSpec (main, spec) where

import Data.IntMod
import Data.Semigroup
import Test.Prelude
import Test.Prop.MonoidAction

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "MonoidAction () (Product IntMod)" $ do
    monoidActionSpec (Proxy @()) (Proxy @(Product IntMod))
  describe "MonoidAction (Sum Int) Int" $ do
    monoidActionSpec (Proxy @(Sum Int)) (Proxy @Int)
  describe "MonoidAction (Product Int) Int" $ do
    monoidActionSpec (Proxy @(Product Int)) (Proxy @Int)
  describe "MonoidAction (Product Int) (Sum Int)" $ do
    monoidActionSpec (Proxy @(Product Int)) (Proxy @(Sum Int))
  describe "MonoidAction (Max Int) Int" $ do
    monoidActionSpec (Proxy @(Max Int)) (Proxy @Int)
  describe "MonoidAction (Min Int) Int" $ do
    monoidActionSpec (Proxy @(Min Int)) (Proxy @Int)
