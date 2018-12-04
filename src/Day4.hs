module Day4 where

import qualified Common                        as C
import qualified Data.List                     as List
import qualified Data.List.Split               as List
import qualified Data.Map                      as M
import           Data.Maybe                     ( catMaybes )
import qualified Text.Parsec                   as P
import           Text.ParserCombinators.Parsec  ( Parser )

data RawData = RawData String Int Int Event deriving Show

instance Eq RawData where
    (RawData s1 _ _ _) == (RawData s2 _ _ _) = s1 == s2

instance Ord RawData where
    (RawData s1 _ _ _) `compare` (RawData s2 _ _ _) = s1 `compare` s2

data Event = Start Int | Asleep Int Int | Awake Int Int deriving Show

partOne :: IO (Maybe Int, Int, Int)
partOne = do
  inp <- traverse (C.parse shiftParser) . lines <$> readFile "data/day4.txt"
  pure $ case inp of
    Left  _ -> (Nothing, 0, 0)
    Right s -> mostAsleepMinute . minutesAsleep $ foldShifts Nothing
                                                             (List.sort s)
                                                             M.empty

partTwo :: IO (Int, Int, Int)
partTwo = do
  inp <- traverse (C.parse shiftParser) . lines <$> readFile "data/day4.txt"
  pure $ case inp of
    Left _ -> (0, 0, 0)
    Right s ->
      maxFrequency . frequencies $ foldShifts Nothing (List.sort s) M.empty

maxFrequency :: M.Map Int (Int, Int) -> (Int, Int, Int)
maxFrequency = M.foldrWithKey
  (\k (m, f) (k', m', f') -> if f > f' then (k, m, f) else (k', m', f'))
  (0, 0, 0)

frequencies :: M.Map Int [Event] -> M.Map Int (Int, Int)
frequencies = M.map
  (\events ->
    let (_, m, f) = mostAsleepMinute (Just 0, 0, flatten events) in (m, f)
  )

foldShifts :: Maybe Int -> [RawData] -> M.Map Int [Event] -> M.Map Int [Event]
foldShifts _       []                    shifts = shifts
foldShifts guardId (RawData _ _ _ e : t) shifts = case (guardId, e) of
  (Nothing, Start guardId') -> foldShifts (Just guardId') t shifts
  (Just _ , Start guardId') -> foldShifts (Just guardId') t shifts
  (Just guardId', ev) ->
    foldShifts (Just guardId') t $ case shifts M.!? guardId' of
      Just _  -> M.update (\events -> Just $ events <> [ev]) guardId' shifts
      Nothing -> M.insert guardId' [ev] shifts
  (Nothing, _) -> error "oh dear"

minutesAsleep :: M.Map Int [Event] -> (Maybe Int, Int, [(Int, Int)])
minutesAsleep = M.foldrWithKey
  (\k events (k', mx, r) ->
    let ranges = flatten events
        mins   = minutesAsleep' ranges
    in  if mins > mx then (Just k, mins, ranges) else (k', mx, r)
  )
  (Nothing, 0, [])

mostAsleepMinute :: (Maybe Int, Int, [(Int, Int)]) -> (Maybe Int, Int, Int)
mostAsleepMinute (Nothing, _, _) = error "oh dear"
mostAsleepMinute (Just guardId, _, ranges) =
  let (m, mx) = foldr
        (\m (m', mx) ->
          let n = length $ filter (inRange m) ranges
          in  if n > mx then (m, n) else (m', mx)
        )
        (0, 0)
        [0 .. 59]
  in  (Just guardId, m, mx)
 where
  inRange :: Int -> (Int, Int) -> Bool
  inRange n (f, t) = n >= f && n < t

minutesAsleep' :: [(Int, Int)] -> Int
minutesAsleep' = foldr (\(f, t) total -> total + (t - f)) 0

flatten :: [Event] -> [(Int, Int)]
flatten events = catMaybes $ toRange <$> List.chunksOf 2 events

toRange :: [Event] -> Maybe (Int, Int)
toRange [Asleep _ m, Awake _ m'] = Just (m, m')
toRange _                        = Nothing


shiftParser :: Parser RawData
shiftParser = do
  y   <- P.char '[' *> P.manyTill P.anyChar (P.char '-')
  m   <- P.manyTill P.anyChar (P.char '-')
  d   <- P.manyTill P.anyChar (P.char ' ')
  h   <- P.manyTill P.anyChar (P.char ':')
  min <- P.manyTill P.anyChar (P.char ']')
  _   <- P.char ' '
  e   <- eventParser (read h) (read min)
  pure $ RawData (y <> "-" <> m <> "-" <> d <> " " <> h <> ":" <> min)
                 (read h)
                 (read min)
                 e

eventParser :: Int -> Int -> Parser Event
eventParser h min = P.choice
  [ Start <$> (P.string "Guard #" *> C.numberParser <* P.string " begins shift")
  , Asleep h min <$ P.string "falls asleep"
  , Awake h min <$ P.string "wakes up"
  ]

