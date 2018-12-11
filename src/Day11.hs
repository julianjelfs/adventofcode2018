module Day11 where

import           Data.List                      ( sum )
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Data.Maybe                     ( catMaybes )

partOne :: ((Int, Int, Int), Int)
partOne = mostPowerfulSquare 3

partTwo :: ((Int, Int, Int), Int)
partTwo = foldr (\(c, p) (c', p') -> if p > p' then (c, p) else (c', p'))
                ((0, 0, 0), minBound)
                (mostPowerfulSquare <$> [1 .. 20])

mostPowerfulSquare :: Int -> ((Int, Int, Int), Int)
mostPowerfulSquare size = M.foldrWithKey
  (\(x, y) p (c', mx) -> if p > mx then ((x, y, size), p) else (c', mx))
  ((0, 0, size), minBound)
  (M.mapWithKey (powerOfSquare grid size) grid)

grid :: Map (Int, Int) Int
grid =
  M.fromList [ ((x, y), power 1718 x y) | x <- [1 .. 300], y <- [1 .. 300] ]

power :: Int -> Int -> Int -> Int
power serial x y = power'
 where
  rackId :: Int
  rackId = x + 10

  power' :: Int
  power' = extract100 ((rackId * y + serial) * rackId) - 5

  extract100 :: Int -> Int
  extract100 n = n `div` 100 `rem` 10

powerOfSquare :: Map (Int, Int) Int -> Int -> (Int, Int) -> Int -> Int
powerOfSquare g size (x, y) _ =
  sum . catMaybes $ flip M.lookup g <$> squareFromCell x y size

squareFromCell :: Int -> Int -> Int -> [(Int, Int)]
squareFromCell x y size =
  [ (x, y)
  | x <- [x .. x + (size - 1)]
  , y <- [y .. y + (size - 1)]
  , x <= 300
  , y <= 300
  ]
