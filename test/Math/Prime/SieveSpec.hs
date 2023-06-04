module Math.Prime.SieveSpec (main, spec) where

import qualified Data.Vector.Unboxed as U
import Math.Prime
import Math.Prime.Sieve
import Test.Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "withPrimes 46337 generates smallPrimes" $
    it "equal to smallPrimes" $
      withPrimes 46337 U.toList `shouldBe` smallPrimes
  describe "buildMoebiusTable" $ do
    it "equal to naive moebius" $ do
      buildMoebiusTable 97 `shouldBe` U.generate 98 moebius
