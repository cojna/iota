module Main where

import           Criterion.Main
import qualified Data.IntMod.OperatorBench as IntModOperator

main :: IO ()
main = defaultMain
    [ IntModOperator.benchMain
    ]
