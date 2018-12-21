{-# LANGUAGE ViewPatterns #-}
module Day14 where

import           Data.Foldable                  ( toList )
import           Data.Sequence                  ( Seq
                                                , (><)
                                                )
import qualified Data.Sequence                 as S

import qualified Data.List.Extra               as L

initial :: (Int, Int, Seq Int)
initial = (0, 1, S.fromList [3, 7])

partTwo :: [Int] -> Int
partTwo input = length . (L.dropEnd 7) . toList $ makeRecipe input [] initial

makeRecipe :: [Int] -> [Int] -> (Int, Int, Seq Int) -> Seq Int
makeRecipe input recent (e1, e2, recipes) =
  let e1Recipe  = S.index recipes e1
      e2Recipe  = S.index recipes e2
      newRecipe = splitDigits $ e1Recipe + e2Recipe
      recipes'  = recipes >< newRecipe
      l         = S.length recipes'
      nextParam = (newIndex e1 e1Recipe l, newIndex e2 e2Recipe l, recipes')
  in  if finished input recent
        then recipes
        else makeRecipe input (moveWindow input recent newRecipe) nextParam

moveWindow :: [Int] -> [Int] -> Seq Int -> [Int]
moveWindow (length -> n) recent recipes =
  let window = recent <> toList recipes
      l      = length window
  in  if l > (n + 1) then drop (l - (n + 1)) window else window


finished :: [Int] -> [Int] -> Bool
finished input recent | input == recent                    = True
                      | input `L.isPrefixOf` recent        = True
                      | input `L.isPrefixOf` drop 1 recent = True
                      | otherwise                          = False

newIndex :: Int -> Int -> Int -> Int
newIndex c i l = (c + i + 1) `mod` l

splitDigits :: Int -> Seq Int
splitDigits i | i < 10    = S.singleton i
              | otherwise = S.fromList [i `div` 10, i `rem` 10]
