module Day14 where

import           Data.Sequence                  ( Seq
                                                , (><)
                                                )
import qualified Data.Sequence                 as S

solution :: Int -> Seq Int
solution n =
  S.take 10 . S.drop n $ makeRecipes (n + 10) (0, 1, S.fromList [3, 7])


makeRecipes :: Int -> (Int, Int, Seq Int) -> Seq Int
makeRecipes target (e1, e2, recipes) =
  let e1Recipe  = S.index recipes e1
      e2Recipe  = S.index recipes e2
      newRecipe = splitDigits $ e1Recipe + e2Recipe
      recipes'  = recipes >< newRecipe
      l         = S.length recipes'
  in  if l >= target
        then recipes'
        else makeRecipes
          target
          (newIndex e1 e1Recipe l, newIndex e2 e2Recipe l, recipes')

newIndex :: Int -> Int -> Int -> Int
newIndex c i l = (c + i + 1) `mod` l

splitDigits :: Int -> Seq Int
splitDigits i | i < 10    = S.singleton i
              | otherwise = S.fromList [i `div` 10, i `rem` 10]
