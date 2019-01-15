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

solution :: IO (Int, Int)
solution = do
  map <- parseText . lines <$> readFile "data/day18.txt"
  pure (evolve 10 map, evolve 1000000000 map)

evolve :: Int -> M.Map Coord Acre -> Int
evolve iterations map =
  let map'   = L.foldl' (\m _ -> eachMinute m) map [1 .. iterations]
      (t, l) = treesOrLumber map'
  in  t * l

treesOrLumber :: M.Map Coord Acre -> (Int, Int)
treesOrLumber = M.foldr
  (\a (t, l) -> case a of
    Tree       -> (t + 1, l)
    Lumberyard -> (t, l + 1)
    _          -> (t, l)
  )
  (0, 0)

eachMinute :: M.Map Coord Acre -> M.Map Coord Acre
eachMinute map = M.mapWithKey (modifyAcre map) map

modifyAcre :: M.Map Coord Acre -> Coord -> Acre -> Acre
modifyAcre map (nOrMoreTrees 3 . surroundingCells map -> change) OpenGround =
  if change then Tree else OpenGround
modifyAcre map (nOrMoreLumberyard 3 . surroundingCells map -> change) Tree =
  if change then Lumberyard else Tree
modifyAcre map (oneLumberAndOneTree . surroundingCells map -> remain) Lumberyard
  = if remain then Lumberyard else OpenGround


parseText :: [String] -> M.Map Coord Acre
parseText lines = parseRows $ zip [0 ..] lines

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


testGrid :: M.Map Coord Acre
testGrid = parseText testLines
 where
  testLines =
    [ ".#.#...|#."
    , ".....#|##|"
    , ".|..|...#."
    , "..|#.....#"
    , "#.#|||#|#|"
    , "...#.||..."
    , ".|....|..."
    , "||...#|.#|"
    , "|.||||..|."
    , "...#.|..|."
    ]
