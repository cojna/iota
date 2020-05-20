module System.Random.XoRoShiRoSpec (main, spec) where

import qualified Data.List               as L
import qualified Data.Vector             as V
import           System.Random.XoRoShiRo
import           Test.Prelude            hiding (shuffle)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "shuffle" $ do
        prop "permutation" prop_permutation

shuffleList :: [a] -> [a]
shuffleList = V.toList . shuffle . V.fromList

prop_permutation :: [Int] -> Bool
prop_permutation xs = L.sort (shuffleList xs) == L.sort xs
