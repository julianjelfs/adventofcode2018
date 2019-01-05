{-# LANGUAGE TupleSections #-}
module Day17 where

import qualified Common                        as C
import qualified Data.Map                      as M
import qualified Text.Parsec                   as P
import           Text.ParserCombinators.Parsec  ( Parser )

data Ground
    = Clay
    | Sand
    | RestingWater
    | FlowingWater
    deriving Show

type Coord = (Int, Int)

solution :: IO (Either P.ParseError (M.Map Coord Ground))
solution = parseVeins . lines <$> readFile "data/day17.txt"

parseVeins :: [String] -> Either P.ParseError (M.Map Coord Ground)
parseVeins lines = M.fromList . concat <$> traverse (C.parse parseVein) lines

parseVein :: Parser [(Coord, Ground)]
parseVein = do
  a     <- P.anyChar <* P.char '='
  ca    <- C.numberParser <* P.string ", "
  _     <- P.anyChar <* P.char '='
  bfrom <- C.numberParser
  _     <- P.string ".."
  bto   <- C.numberParser
  pure $ (, Clay) <$> case a of
    'x' -> [ (ca, y) | y <- [bfrom .. bto] ]
    'y' -> [ (x, ca) | x <- [bfrom .. bto] ]
    _   -> error "unexpected input found"

testGrid :: [String]
testGrid =
  [ "x=495, y=2..7"
  , "y=7, x=495..501"
  , "x=501, y=3..7"
  , "x=498, y=2..4"
  , "x=506, y=1..2"
  , "x=498, y=10..13"
  , "x=504, y=10..13"
  , "y=13, x=498..504"
  ]
