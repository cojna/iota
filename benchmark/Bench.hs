module Main where

import Criterion.Main
import qualified Data.BSRBench as BSR
import qualified Data.IntModBench as IntMod

main :: IO ()
main =
  defaultMain
    [ IntMod.benchMain
    , BSR.benchMain
    ]
