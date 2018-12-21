{-# LANGUAGE LambdaCase #-}
module Day15 where

import           Data.List                     as L
import           Data.Map                      as M

data Cell = Wall | Goblin | Elf | Space deriving Show
type Coord = (Int, Int)

solution :: IO (M.Map Coord Cell)
solution = do
  g <- grid . lines <$> readFile "data/day15.txt"
  pure g


grid :: [String] -> M.Map Coord Cell
grid rows = L.foldl'
  (\m (y, row) ->
    L.foldl' (\m (x, c) -> M.insert (x, y) (parseCell c) m) m (zip [0 ..] row)
  )
  M.empty
  (zip [0 ..] rows)


parseCell :: Char -> Cell
parseCell = \case
  '#' -> Wall
  'G' -> Goblin
  'E' -> Elf
  '.' -> Space
  _   -> error "parse error"

