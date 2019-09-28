module Data.UnionFind.VectorSpec (main, spec) where

import           Data.UnionFind.Vector
import           GHC.Exts
import           Test.Prelude

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
        ne <- equivM uf 0 1
        uniteM uf 0 1
        e <- equivM uf 0 1
        return (ne, e)
    assert (not notEquiv && equiv)

prop_countGroup :: Property
prop_countGroup = monadicIO $ do
    count <- run $ do
        uf <- newUnionFind 5
        uniteM uf 0 1
        uniteM uf 3 4
        uniteM uf 2 1
        countGroupM uf
    assert (count == 2)

