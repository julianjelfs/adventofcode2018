module Main where

import           Day1
import           Day3

main :: IO ()
main = Day3.partOne >>= putStrLn . show
