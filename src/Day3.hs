module Day3 where

import qualified Common                        as C
import qualified Text.Parsec                   as P
import           Text.ParserCombinators.Parsec  ( ParseError
                                                , Parser
                                                )

data Claim = Claim
    { coords :: (Int, Int)
    , dims   :: (Int, Int)
    } deriving (Show)


partOne :: IO Int
partOne = do
  claims <- traverse (C.parse claimParser) . lines <$> readFile "data/day3.txt"
  pure $ case claims of
    Left  err -> 0
    Right c   -> length . filter ((>) 1) $ countOverlaps c <$> coords
 where
  coords :: [(Int, Int)]
  coords = [ (x, y) | x <- [0 .. 999], y <- [0 .. 999] ]

  claimParser :: Parser Claim
  claimParser = do
    _ <- P.char '#'
    _ <- C.numberParser
    _ <- P.string " @ "
    x <- C.numberParser
    _ <- P.char ','
    y <- C.numberParser
    _ <- P.string ": "
    w <- C.numberParser
    _ <- P.char 'x'
    h <- C.numberParser
    pure $ Claim (x, y) (w, h)

countOverlaps :: [Claim] -> (Int, Int) -> Int
countOverlaps claims coord = length $ filter (overlaps coord) claims

overlaps :: (Int, Int) -> Claim -> Bool
overlaps (x, y) (Claim (x', y') (w, h)) =
  (x >= x' && x < (x' + w)) && (y >= y' && y < (y' + h))




