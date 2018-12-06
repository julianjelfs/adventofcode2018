module Day6 where

import qualified Common                        as C
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import qualified Text.Parsec                   as P
import           Text.ParserCombinators.Parsec (Parser)

type Point = (Int, Int)
type GridPoint = (Int, Int)
type Areas = M.Map Point (S.Set GridPoint)

solution :: IO (Int, Int)
solution = do
  inp <- traverse (C.parse coordParser) . lines <$> readFile "data/day6.txt"
  pure $ case inp of
    Left  _ -> (0, 0)
    Right c ->
        let g = grid c
        in
            ( largestArea $ removeInfinites c $ foldr (nearestPoint c) M.empty g
            , length $ filter (safe c) g
            )

grid :: [Point] -> [GridPoint]
grid points = [ (x, y) | x <- [minx .. maxx], y <- [miny .. maxy] ]
  where ((minx, miny), (maxx, maxy)) = getBounds points

safe :: [Point] -> GridPoint -> Bool
safe points point =
    10000 > sum (manhattanDistance point <$> points)

largestArea :: Areas -> Int
largestArea =
    M.foldr
        (\s max ->
            if S.size s > max
            then S.size s
            else max) 0

removeInfinites :: [Point] -> Areas -> Areas
removeInfinites points =
    M.filterWithKey
        (\k _ -> not $ infiniteArea points k)

nearestPoint :: [Point] -> GridPoint -> Areas -> Areas
nearestPoint points gridPoint areas =
    case uniqueMin $ (\p -> (p, manhattanDistance gridPoint p)) <$> points of
        Nothing -> areas
        Just (p, _) ->
            if p `M.member` areas
            then M.update (Just . S.insert gridPoint) p areas
            else M.insert p (S.singleton gridPoint) areas

uniqueMin :: [(Point, Int)] -> Maybe (Point, Int)
uniqueMin points = case foldr
    (\(p, n) (ps, mn) ->
        if n < mn then ([p], n)
        else if n == mn then (p : ps, n)
        else (ps, mn)
    ) ([], maxBound) points of
    ([p], n) -> Just (p, n)
    _        -> Nothing

infiniteArea :: [Point] -> Point -> Bool
infiniteArea points (x, y) =
    let ((minx, miny), (maxx, maxy)) = getBounds points
    in x == minx || x == maxx || y == miny || y == maxy

manhattanDistance :: GridPoint -> Point -> Int
manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

getBounds :: [Point] -> (Point, Point)
getBounds points =
  let (minx, maxx) = minMax $ fst <$> points
      (miny, maxy) = minMax $ snd <$> points
  in  ((minx, miny), (maxx, maxy))

minMax :: Ord a => [a] -> (a, a)
minMax ns = (minimum ns, maximum ns)

coordParser :: Parser Point
coordParser = (,) <$> (C.numberParser <* P.string ", ") <*> C.numberParser
