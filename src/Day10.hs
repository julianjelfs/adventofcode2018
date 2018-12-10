module Day10 where

import qualified Common                        as C
import           Data.List                     (foldl')
import qualified Text.Parsec                   as P
import           Text.ParserCombinators.Parsec (Parser)

data Point = Point Position Velocity deriving Show
data Position = Position Int Int deriving Show
data Velocity = Velocity Int Int deriving Show

solution :: IO ()
solution = do
  inp <- traverse (C.parse instrParser) . lines <$> readFile "data/day10.txt"
  case inp of
    Left _ -> print "error"
    Right points ->
        print . show . (+ 1) . time $ foldl'
            (\(n, b, p) n' ->
                let p' = movePoints p
                    b' = sumBounds . getBounds $ p'
                in  if b' < b
                    then (n', b', p')
                    else (n, b, p)
            ) (0, maxBound, points) [0..20000]

third :: (a,b,c) -> c
third (_, _, c) = c

time :: (a,b,c) -> a
time (a, _, _) = a

movePoints :: [Point] -> [Point]
movePoints = fmap
  (\(Point (Position x y) v@(Velocity dx dy)) ->
    Point (Position (x + dx) (y + dy)) v
  )

sumBounds :: ((Int, Int), (Int, Int)) -> Int
sumBounds ((minx, miny), (maxx, maxy)) =
    (maxx - minx) + (maxy - miny)

showPoints :: [Point] -> String
showPoints points = unlines
  $ fmap (\y' -> foldr (drawLine y') "" [minx .. maxx]) [miny .. maxy]
 where
  ((minx, miny), (maxx, maxy)) = getBounds points

  pos = (\(Point (Position x y) _) -> (x, y)) <$> points

  drawPoint :: Int -> Int -> Char
  drawPoint x y = if (x, y) `elem` pos then '#' else '.'

  drawLine :: Int -> Int -> String -> String
  drawLine y x line = drawPoint x y : line


getBounds :: [Point] -> ((Int, Int), (Int, Int))
getBounds points =
  let xs = (\(Point (Position x _) _) -> x) <$> points
      ys = (\(Point (Position _ y) _) -> y) <$> points
  in  ((minimum xs, minimum ys), (maximum xs, maximum ys))


instrParser :: Parser Point
instrParser = do
  x <-
    P.string "position=<"
    *> P.spaces
    *> C.numberParser
    <* P.char ','
    <* P.spaces
  y  <- C.numberParser <* P.string "> "
  dx <-
    P.string "velocity=<"
    *> P.spaces
    *> C.numberParser
    <* P.char ','
    <* P.spaces
  dy <- C.numberParser <* P.char '>'
  pure $ Point (Position x y) (Velocity dx dy)
