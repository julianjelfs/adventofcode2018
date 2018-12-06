module Day6 where

import qualified Common                        as C
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import qualified Text.Parsec                   as P
import           Text.ParserCombinators.Parsec  ( Parser )

type Point = (Int, Int)
type GridPoint = (Int, Int)
type Areas = M.Map Point (S.Set GridPoint)

partOne :: IO Areas
partOne = do
  inp <- traverse (C.parse coordParser) . lines <$> readFile "data/day6.txt"
  pure $ case inp of
    Left  _ -> M.empty
    Right c -> foldr (nearestPoint c) M.empty (grid c)
        --nearestPoint M.empty c <$> grid c

    --so far we have a map of all points in the grid to the coord they are
    --closest to

grid :: [Point] -> [GridPoint]
grid points = [ (x, y) | x <- [minx .. maxx], y <- [miny .. maxy] ]
  where ((minx, miny), (maxx, maxy)) = getBounds points

nearestPoint :: [Point] -> GridPoint -> Areas -> Areas
nearestPoint points gridPoint areas = undefined
    --manhattanDistance gridPoint <$> points

--a point's are will be infinite if it is not surrounded by other points
isInfinite :: [Point] -> Point -> Bool
isInfinite points point =
  not
    $  any (left point)  points
    && any (right point) points
    && any (up point)    points
    && any (down point)  points
 where
  left :: Point -> Point -> Bool
  left (x1, _) (x2, _) = x2 > x1

  right :: Point -> Point -> Bool
  right (x1, _) (x2, _) = x2 < x1

  up :: Point -> Point -> Bool
  up (_, y1) (_, y2) = y2 < y1

  down :: Point -> Point -> Bool
  down (_, y1) (_, y2) = y2 > y1

manhattanDistance :: GridPoint -> Point -> Int
manhattanDistance (x1, y1) (x2, y2) = (x1 - x2) + (y1 - y2)

getBounds :: [Point] -> (Point, Point)
getBounds points =
  let (minx, maxx) = minMax $ fst <$> points
      (miny, maxy) = minMax $ snd <$> points
  in  ((minx, miny), (maxx, maxy))

minMax :: Ord a => [a] -> (a, a)
minMax ns = (minimum ns, maximum ns)

coordParser :: Parser Point
coordParser = (,) <$> (C.numberParser <* P.string ", ") <*> C.numberParser
