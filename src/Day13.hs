{-# LANGUAGE RecordWildCards #-}

module Day13 where

import qualified Data.List as L
import           Data.Map  (Map)
import qualified Data.Map  as M
import           Data.Set  (Set)
import qualified Data.Set  as S

type Coord = (Int,Int)

data Car = Car Coord NextTurn Direction deriving Show

instance Eq Car where
    (Car c _ _) == (Car c' _ _) = c == c'

instance Ord Car where
    (Car c _ _) `compare` (Car c' _ _) = c `compare` c'

data Direction = U | D | L | R deriving Show

data NextTurn = TurnLeft | TurnRight | GoStraight deriving Show

data Track
    = Vertical
    | Horizontal
    | Backslash
    | Forwardslash
    | Intersection
    deriving Show

data State = State
    { cars  :: Set Car
    , track :: Map Coord Track
    } deriving Show

solution :: IO (Maybe Coord)
solution =
  eventLoop . parseState . lines <$> readFile "data/day13.txt"

eventLoop :: State -> Maybe Coord
eventLoop State {..} =
    let updatedCars =
            L.foldl'
                (\cars' car ->
                      let car'@(Car c' _ _) = moveCar track car
                      in case M.lookup c' cars' of
                        Nothing -> M.insert c' [car'] cars'
                        Just _  -> M.update (\l -> Just $ car' : l) c' cars'
                ) M.empty (L.sort $ S.toList cars)
        deduped = dedupeCars updatedCars
    in case M.size deduped of
            1 -> lastCarCoord deduped
            _ -> eventLoop (State { cars = concatCars deduped, ..})

dedupeCars :: Map Coord [a] -> Map Coord [a]
dedupeCars = M.filter (\cars -> L.length cars == 1)

concatCars :: Ord a => Map Coord [a] -> Set a
concatCars = S.fromList . M.foldr' (<>) []

lastCarCoord :: Map Coord [Car] -> Maybe Coord
lastCarCoord m =
    case M.toList m of
        [(c, _)] -> Just c
        _        -> Nothing

moveCar :: Map Coord Track -> Car -> Car
moveCar t (Car (x,y) n U) =
    case M.lookup (x, y-1) t of
        Just Vertical -> Car (x, y-1) n U
        Just Backslash -> Car (x, y-1) n L
        Just Forwardslash -> Car (x, y-1) n R
        Just Intersection -> Car (x, y-1) (nextTurn n) (makeTurn n U)
        _ -> error "the track and the car are in a fucked up state"
moveCar t (Car (x,y) n D) =
    case M.lookup (x, y+1) t of
        Just Vertical -> Car (x, y+1) n D
        Just Backslash -> Car (x, y+1) n R
        Just Forwardslash -> Car (x, y+1) n L
        Just Intersection -> Car (x, y+1) (nextTurn n) (makeTurn n D)
        _ -> error "the track and the car are in a fucked up state"
moveCar t (Car (x,y) n L) =
    case M.lookup (x-1, y) t of
        Just Horizontal -> Car (x-1, y) n L
        Just Backslash -> Car (x-1, y) n U
        Just Forwardslash -> Car (x-1, y) n D
        Just Intersection -> Car (x-1, y) (nextTurn n) (makeTurn n L)
        _ -> error "the track and the car are in a fucked up state"
moveCar t (Car (x,y) n R) =
    case M.lookup (x+1, y) t of
        Just Horizontal -> Car (x+1, y) n R
        Just Backslash -> Car (x+1, y) n D
        Just Forwardslash -> Car (x+1, y) n U
        Just Intersection -> Car (x+1, y) (nextTurn n) (makeTurn n R)
        _ -> error "the track and the car are in a fucked up state"

makeTurn :: NextTurn -> Direction -> Direction
makeTurn TurnLeft U   = L
makeTurn TurnLeft D   = R
makeTurn TurnLeft L   = D
makeTurn TurnLeft R   = U
makeTurn TurnRight U  = R
makeTurn TurnRight D  = L
makeTurn TurnRight L  = U
makeTurn TurnRight R  = D
makeTurn GoStraight d = d

nextTurn :: NextTurn -> NextTurn
nextTurn TurnLeft   = GoStraight
nextTurn GoStraight = TurnRight
nextTurn TurnRight  = TurnLeft

parseState :: [String] -> State
parseState lines = L.foldl' rows (State S.empty M.empty) (zip [0 ..] lines)
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
    'v'  -> addTrack (x, y) Vertical (addCar (x, y) D s)
    '^'  -> addTrack (x, y) Vertical (addCar (x, y) U s)
    '+'  -> addTrack (x, y) Intersection s
    _    -> s

  addTrack :: Coord -> Track -> State -> State
  addTrack c t s = s { track = M.insert c t (track s) }

  addCar :: Coord -> Direction -> State -> State
  addCar c d s = s { cars = S.insert (Car c TurnLeft d) (cars s) }

