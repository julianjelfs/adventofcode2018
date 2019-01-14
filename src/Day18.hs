{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE ViewPatterns #-}
module Day18 where

import qualified Data.List                     as L
import qualified Data.Map                      as M
import           Data.Maybe                     ( catMaybes )

data Acre
    = OpenGround
    | Tree
    | Lumberyard
    deriving Show

type Coord = (Int, Int)

solution :: IO (M.Map Coord Acre)
solution = do
  map <- parseRows . zip [0 ..] . lines <$> readFile "data/day18.txt"
  pure map

parseRows :: [(Int, String)] -> M.Map Coord Acre
parseRows = L.foldl'
  (\m (y, row) ->
    L.foldl' (\m (x, c) -> M.insert (x, y) (parseChar c) m) m (zip [0 ..] row)
  )
  M.empty

surroundingCells :: M.Map Coord Acre -> Coord -> [Acre]
surroundingCells m c = catMaybes $ (`M.lookup` m) <$> surroundingCoords c

surroundingCoords :: Coord -> [Coord]
surroundingCoords c@(x, y) =
  filter (/= c) [ (x', y') | x' <- [x - 1 .. x + 1], y' <- [y - 1 .. y + 1] ]


nOrMoreTrees :: Int -> [Acre] -> Bool
nOrMoreTrees n (length . filter isTree -> l) = l >= n

nOrMoreLumberyard :: Int -> [Acre] -> Bool
nOrMoreLumberyard n (length . filter isLumberyard -> l) = l >= n

oneLumberAndOneTree :: [Acre] -> Bool
oneLumberAndOneTree acres = nOrMoreTrees 1 acres && nOrMoreLumberyard 1 acres

isTree :: Acre -> Bool
isTree Tree = True
isTree _    = False

isLumberyard :: Acre -> Bool
isLumberyard Lumberyard = True
isLumberyard _          = False

parseChar :: Char -> Acre
parseChar = \case
  '.' -> OpenGround
  '|' -> Tree
  '#' -> Lumberyard
  _   -> error "Unexpected character received"
