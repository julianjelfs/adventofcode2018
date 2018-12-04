module Main where

import           Day4

main :: IO ()
main = do
  p1 <- Day4.partOne
  p2 <- Day4.partTwo
  putStrLn . show $ p1
  putStrLn . show $ p2
