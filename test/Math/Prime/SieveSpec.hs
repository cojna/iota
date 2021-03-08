module Math.Prime.SieveSpec (main, spec) where

import           Data.Int
import qualified Data.List           as L
import qualified Data.Vector.Unboxed as U
import           Data.Word
import           Math.Prime
import           Math.Prime.Sieve
import           Test.Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "withPrimes 46337 generates smallPrimes" $
        it "equal to smallPrimes" $
            withPrimes 46337 U.toList `shouldBe` smallPrimes
