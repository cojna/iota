module Main where

import           Criterion.Main
import qualified Data.IntModBench as IntMod

main :: IO ()
main = defaultMain
    [ IntMod.benchMain
    ]
