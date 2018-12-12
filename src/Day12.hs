module Day12 where

import qualified Common                        as C
import qualified Data.List                     as List
import qualified Data.Vector                   as V
import qualified Text.Parsec                   as P
import           Text.ParserCombinators.Parsec (Parser)

data Note = Note String Char deriving Show

solution :: IO (Int, String)
solution = do
  inp <- traverse (C.parse noteParser) . lines <$> readFile "data/day12.txt"
  pure $ case inp of
    Left  err   -> error $ show err
    Right notes -> sumState $ foldr (evolve notes) (0, initialState) [1..20]

initialState :: String
initialState
  = "#.#..#..###.###.#..###.#####...########.#...#####...##.#....#.####.#.#..#..#.#..###...#..#.#....##."

firstNote :: Note
firstNote = Note "...#." '#'

evolve :: [Note] -> Int -> (Int, String) -> (Int, String)
evolve notes _ (start, state) =
    let state' = "....." <> state <> "....."
        target = V.toList $ List.foldl'
            (\target' (i, _) ->
                let slice = take 5 . drop i $ state'
                in case matchingNote notes slice of
                    Nothing          -> target'
                    Just (Note _ c') -> V.update target' (V.fromList [((i+2), c')])
            ) (V.fromList state') (zip [0..(length state' - 5)] state')
    in (newStart start target, dropWhile (== '.') target)

newStart :: Int -> String -> Int
newStart start state =
    case List.elemIndex '#' state of
        Nothing -> start
        Just n  -> start + n - 5

matchingNote :: [Note] -> String -> Maybe Note
matchingNote notes slice =
    case filter (\(Note pattern _) -> pattern == slice) notes of
        []    -> Nothing
        (n:_) -> Just n

sumState :: (Int, String) -> (Int, String)
sumState (start, state) =
    ( sum $ (\(n, c) -> if c == '#' then n else 0) <$> zip [start..] state
    , state )

noteParser :: Parser Note
noteParser = do
  pattern <- P.manyTill P.anyChar P.space <* P.string "=> "
  onoff   <- P.anyChar
  pure $ Note pattern onoff
