module Day12 where

import qualified Common                        as C
import qualified Data.List                     as List
import qualified Data.Set                      as S
import qualified Data.Vector                   as V
import qualified Text.Parsec                   as P
import           Text.ParserCombinators.Parsec (Parser)

data Note = Note String Char deriving Show

--after 1000 score is 73377
--each subsequent iteration increases score by 73
--therefore after 50000000000 iterations
--the total will be 3650000000377
--
solution :: IO ((Int, Int, String), [Int])
solution = do
  inp <- traverse (C.parse noteParser) . lines <$> readFile "data/day12.txt"
  pure $ case inp of
    Left  err   -> error $ show err
    Right notes ->
        ( List.foldl' (evolve notes) (0, 0, initialState)  [1..20]
        , snd' <$> List.scanl' (evolve notes) (0, 0, initialState) [1..1000]
        )

snd' :: (a,b,c) -> b
snd' (_,b,_) = b

setify :: [(Int, Int, String)] -> Int
setify results =
    S.size . S.fromList $ (\(start, _, str) -> (start, str)) <$> results

numberOfStates :: (Int, String, S.Set Int) -> Int
numberOfStates (_, _, s) = S.size s

initialState :: String
initialState
  = "#.#..#..###.###.#..###.#####...########.#...#####...##.#....#.####.#.#..#..#.#..###...#..#.#....##"

evolve :: [Note] -> (Int, Int, String) -> Int -> (Int, Int, String)
evolve notes (start, _, state) _ =
    let state' = "....." <> state <> "....."
        target = V.toList $ List.foldl'
            (\target' (i, _) ->
                let slice = take 5 . drop i $ state'
                in case matchingNote notes slice of
                    Nothing          -> target'
                    Just (Note _ c') -> V.update target' (V.fromList [((i+2), c')])
            ) (V.fromList state') (zip [0..(length state' - 5)] state')
        trimmed = List.dropWhileEnd (== '.') . dropWhile (== '.') $ target
        start' = newStart start target
        score = sumState (start', trimmed)
    in (start', score, trimmed)

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

sumState :: (Int, String) -> Int
sumState (start, state) =
    sum $ (\(n, c) -> if c == '#' then n else 0) <$> zip [start..] state

noteParser :: Parser Note
noteParser = do
  pattern <- P.manyTill P.anyChar P.space <* P.string "=> "
  onoff   <- P.anyChar
  pure $ Note pattern onoff
