module Data.UnionFindSpec (main, spec) where

import Data.UnionFind
import Test.Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "UnionFind" $ do
    prop "unite" prop_unite
    prop "count group" prop_countGroup

prop_unite :: Property
prop_unite = monadicIO $ do
  (notEquiv, equiv) <- run $ do
    uf <- newUnionFind 5
    ne <- equivUF uf 0 1
    uniteUF uf 0 1
    e <- equivUF uf 0 1
    return (ne, e)
  assert (not notEquiv && equiv)

prop_countGroup :: Property
prop_countGroup = monadicIO $ do
  count <- run $ do
    uf <- newUnionFind 5
    uniteUF uf 0 1
    uniteUF uf 3 4
    uniteUF uf 2 1
    countGroupUF uf
  assert (count == 2)
