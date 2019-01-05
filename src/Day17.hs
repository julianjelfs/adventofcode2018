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

solution :: IO Int
solution = undefined

parseVeins :: IO (Either P.ParseError (M.Map Coord Ground))
parseVeins =
  (fmap . fmap) (M.fromList . concat)
    $   traverse (C.parse parseVein)
    .   lines
    <$> readFile "data/day17.txt"

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
