module Test.Prop.VectorSpace where

import Test.Prelude

scalarSpec ::
  ( Arbitrary v
  , Num v
  , Show v
  , Eq v
  , Arbitrary k
  , Num k
  , Show k
  , Eq k
  ) =>
  (k -> v -> v) ->
  Spec
scalarSpec (*:) =
  describe "scalar multiplication (*:)" $ do
    prop "r *: (x + y) = r *: x + r *: y" $ prop_vectorDistributive (*:)
    prop "(r + s) *: x = r *: x + s *: x" $ prop_scalarDistributive (*:)
    prop "(r * s) *: x = r *: (s *: x)" $ prop_scalarAssociative (*:)
    prop "1 *: x = x" $ prop_scalarIdentity (*:)

prop_vectorDistributive :: (Num v, Eq v) => (k -> v -> v) -> k -> v -> v -> Bool
prop_vectorDistributive (*:) r x y = r *: (x + y) == r *: x + r *: y

prop_scalarDistributive :: (Num v, Eq v, Num k) => (k -> v -> v) -> k -> k -> v -> Bool
prop_scalarDistributive (*:) r s x = (r + s) *: x == r *: x + s *: x

prop_scalarAssociative :: (Num v, Eq v, Num k) => (k -> v -> v) -> k -> k -> v -> Bool
prop_scalarAssociative (*:) r s x = (r * s) *: x == r *: (s *: x)

prop_scalarIdentity :: (Num v, Eq v, Num k) => (k -> v -> v) -> v -> Bool
prop_scalarIdentity (*:) x = 1 *: x == x
