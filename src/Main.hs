module Main where

import           Day1

main :: IO ()
main = Day1.solve >>= putStrLn . show
