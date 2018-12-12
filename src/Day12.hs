module Day12 where

import qualified Common                        as C
import qualified Data.List.Utils               as List
import qualified Text.Parsec                   as P
import           Text.ParserCombinators.Parsec  ( Parser )

data Note = Note String String deriving Show

solution :: IO [Note]
solution = do
  inp <- traverse (C.parse noteParser) . lines <$> readFile "data/day12.txt"
  pure $ case inp of
    Left  err -> error $ show err
    Right i   -> i

noteParser :: Parser Note
noteParser = do
  pattern <- P.manyTill P.anyChar P.space <* P.string "=> "
  onoff   <- replaceWith pattern <$> P.anyChar
  pure $ Note pattern onoff

initialState :: String
initialState
  = "#.#..#..###.###.#..###.#####...########.#...#####...##.#....#.####.#.#..#..#.#..###...#..#.#....##."


testState :: String
testState = "#..#.#..##......###...###"

replaceWith :: String -> Char -> String
replaceWith pattern next = take 2 pattern <> [next] <> drop 3 pattern
