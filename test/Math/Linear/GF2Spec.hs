{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Math.Linear.GF2Spec (main, spec) where

import Data.Coerce
import qualified Data.Vector.Unboxed as U
import GHC.Exts (fromList)
import Math.Linear.GF2
import Test.Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "rankGF2x64'" $ do
    specify "rankGF2x64' zeroGH2x64 == 0" $ do
      rankGF2x64' zeroGF2x64' `shouldBe` 0
    specify "rankGF2x64' [0b1] == 1" $ do
      rankGF2x64' [0b1] `shouldBe` 1
    specify "rankGF2x64' [0b01, 0b11] == 2" $ do
      rankGF2x64' [0b01, 0b11] `shouldBe` 2
    specify "rankGF2x64' [0b01, 0b11, 0b10] == 2" $ do
      rankGF2x64' [0b01, 0b11, 0b10] `shouldBe` 2
  describe "spanGF2x64'" $ do
    prop "span" prop_span
  describe "inGF2x64'" $ do
    prop "0 in GF2x64'" (inGF2x64' 0)
  describe "componentsGF2x64'" $ do
    prop "linear independent" prop_linear_independent
  describe "linCombGF2x64' vs . componentsGF2x64 vs = id" $ do
    prop "combComp == i" prop_comb_comp
    describe "span by [0b001, 0b111, 0b010]" $ do
      let !base = spanGF2x64' [0b001, 0b111, 0b010]
          combComp = linCombGF2x64' base . componentsGF2x64' base
      specify "combComp 0b000 == 0b000" $ do
        combComp 0b000 `shouldBe` 0b000
      specify "combComp 0b001 == 0b001" $ do
        combComp 0b001 `shouldBe` 0b001
      specify "combComp 0b010 == 0b100" $ do
        combComp 0b010 `shouldBe` 0b010
      specify "combComp 0b011 == 0b011" $ do
        combComp 0b011 `shouldBe` 0b011
      specify "combComp 0b100 == 0b100" $ do
        combComp 0b100 `shouldBe` 0b100
      specify "combComp 0b101 == 0b101" $ do
        combComp 0b101 `shouldBe` 0b101
      specify "combComp 0b110 == 0b110" $ do
        combComp 0b110 `shouldBe` 0b110
      specify "combComp 0b111 == 0b111" $ do
        combComp 0b111 `shouldBe` 0b111

instance Arbitrary GF2x64 where
  arbitrary = coerce (arbitrary @Word)

instance Arbitrary GF2x64' where
  arbitrary = fromList <$> arbitrary

prop_span :: [GF2x64] -> Bool
prop_span (U.fromList -> vs) = U.all (`inGF2x64'` bs) vs
  where
    !bs = spanGF2x64' vs

prop_linear_independent :: GF2x64' -> Bool
prop_linear_independent vs =
  componentsGF2x64' vs 0 == 0

prop_comb_comp :: GF2x64' -> GF2x64 -> Bool
prop_comb_comp vs v =
  not (inGF2x64' v vs)
    || linCombGF2x64' vs (componentsGF2x64' vs v) == v
