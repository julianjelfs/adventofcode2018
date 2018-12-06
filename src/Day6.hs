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
    let res = foldr
            (\p agg ->
                let m = manhattanDistance gridPoint p
                in case agg of
                    Nothing -> Just ([p], m)
                    Just (ps, m')
                        | m < m' -> Just ([p], m)
                        | m == m' -> Just (p:ps, m)
                        | otherwise -> agg
            ) Nothing points
    in case res of
        Just ([p], _) ->
            if p `M.member` areas
            then M.update (Just . S.insert gridPoint) p areas
            else M.insert p (S.singleton gridPoint) areas
        _             -> areas

infiniteArea :: [Point] -> Point -> Bool
infiniteArea points point =
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
