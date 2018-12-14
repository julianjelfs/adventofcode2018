module Day14 where

import qualified Data.List.Extra as L
--import           Debug.Trace     (traceShowId)

-- solution :: Int -> (Seq Int, Int)
-- solution n =
--   (partOne, 0)
--   where
--     _input = [5,1,5,8,9]

--     partOne = S.take 10 . S.drop n $ makeRecipesUntil (\s -> S.length s >= (n + 10)) initial
--
--  probably we want to build this in reverse order

initial :: (Int, Int, [Int])
initial = (0,1,[3,7])

partTwo :: [Int] -> Int
partTwo input = length . drop (length input) $ makeRecipesUntil input initial

makeRecipesUntil :: [Int] -> (Int, Int, [Int]) -> [Int]
makeRecipesUntil input (e1, e2, recipes) =
  let e1Recipe  = recipes !! e1
      e2Recipe  = recipes !! e2
      newRecipe = splitDigits $ e1Recipe + e2Recipe
      recipes'  = recipes <> newRecipe
      l         = length recipes'
      nextParam = (newIndex e1 e1Recipe l, newIndex e2 e2Recipe l, recipes')

  in if L.takeEnd (length input) recipes'  == input
     then recipes'
     else makeRecipesUntil input nextParam

newIndex :: Int -> Int -> Int -> Int
newIndex c i l = (c + i + 1) `mod` l

splitDigits :: Int -> [Int]
splitDigits i | i < 10    = [i]
              | otherwise = [i `div` 10, i `rem` 10]
