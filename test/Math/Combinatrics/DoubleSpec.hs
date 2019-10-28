module Math.Combinatrics.DoubleSpec where

import           Data.Double.EPS
import           Math.Combinatrics
import           Math.Combinatrics.Double
import           Test.Prelude

main :: IO ()
main = hspec spec

eq :: Double -> Double -> Bool
eq = eqEPS 1e-9

spec :: Spec
spec = do
    describe "logFact" $ do
        it "exp (logFact 0) == 1" $ do
            exp (logFact 0) `shouldSatisfy` eq 1.0
        it "exp (logFact 1) == 1" $ do
            exp (logFact 1) `shouldSatisfy` eq 1.0
        it "exp (logFact 2) == 2" $ do
            exp (logFact 2) `shouldSatisfy` eq 2.0
        it "exp (logFact 10) == 3628800" $ do
            exp (logFact 10) `shouldSatisfy` eq 3628800.0
    describe "logPerm" $ do
        it "exp (logPerm 10 0) == 3628800" $ do
            exp (logPerm 10 0) `shouldSatisfy` eq 3628800.0
        it "exp (logPrem 10 1) == 3628800" $ do
            exp (logPerm 10 1) `shouldSatisfy` eq 3628800.0
        it "exp (logPerm 10 2) == 1814400" $ do
            exp (logPerm 10 2) `shouldSatisfy` eq 1814400.0
        it "exp (logPerm 10 9) == 10" $ do
            exp (logPerm 10 9) `shouldSatisfy` eq 10.0
        it "exp (logPerm 10 10) == 1" $ do
            exp (logPerm 10 10) `shouldSatisfy` eq 1.0
    describe "logComb" $ do
        it "exp (logComb 10 0) == 1" $ do
            exp (logComb 10 0) `shouldSatisfy` eq 1.0
        it "exp (logComb 10 1) == 10" $ do
            exp (logComb 10 1) `shouldSatisfy` eq 10.0
        it "exp (logComb 10 2) == 45" $ do
            exp (logComb 10 2) `shouldSatisfy` eq 45.0
        it "exp (logComb 10 10) == 1" $ do
            exp (logComb 10 10) `shouldSatisfy` eq 1.0

