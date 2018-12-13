{-# LANGUAGE RecordWildCards #-}

module Day13 where

import qualified Data.List                     as L
import           Data.Map                       ( Map )
import qualified Data.Map                      as M

type Coord = (Int,Int)

data Car = Car Coord Direction deriving Show

data Direction = U | D | L | R deriving Show

data Track
    = Vertical
    | Horizontal
    | Backslash
    | Forwardslash
    | Intersection
    deriving Show

data State = State
    { cars  :: [Car]
    , track :: Map Coord Track
    } deriving Show

solution :: IO Coord
solution = do
  state <- parseState . lines <$> readFile "data/day13.txt"
  pure $ eventLoop state

eventLoop :: State -> Coord
eventLoop State {..} =
  let cars' = moveCar track <$> cars
  in  case collision cars' of
        Nothing    -> eventLoop (State {cars = cars', ..})
        Just coord -> coord
 where
  moveCar :: Map Coord Track -> Car -> Car
  moveCar _t c = c

  collision :: [Car] -> Maybe Coord
  collision _cars = undefined

parseState :: [String] -> State
parseState lines = L.foldl' rows (State [] M.empty) (zip [0 ..] lines)
 where
  rows :: State -> (Int, String) -> State
  rows s (y, row) = L.foldl' (cols y) s (zip [0 ..] row)

  cols :: Int -> State -> (Int, Char) -> State
  cols y s (x, c) = case c of
    '-'  -> addTrack (x, y) Horizontal s
    '|'  -> addTrack (x, y) Vertical s
    '\\' -> addTrack (x, y) Backslash s
    '/'  -> addTrack (x, y) Forwardslash s
    '>'  -> addTrack (x, y) Horizontal (addCar (x, y) R s)
    '<'  -> addTrack (x, y) Horizontal (addCar (x, y) L s)
    'V'  -> addTrack (x, y) Vertical (addCar (x, y) D s)
    '^'  -> addTrack (x, y) Vertical (addCar (x, y) U s)
    '+'  -> addTrack (x, y) Intersection s
    _    -> s

  addTrack :: Coord -> Track -> State -> State
  addTrack c t s = s { track = M.insert c t (track s) }

  addCar :: Coord -> Direction -> State -> State
  addCar c d s = s { cars = Car c d : cars s }

