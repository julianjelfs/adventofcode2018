module Day4 where

import qualified Common                        as C
import qualified Data.List                     as List
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import qualified Text.Parsec                   as P
import           Text.ParserCombinators.Parsec  ( Parser )

type Minutes = M.Map Int Int

data Shift = Shift String Int Int Event deriving Show

instance Eq Shift where
    (Shift s1 _ _ _) == (Shift s2 _ _ _) = s1 == s2

instance Ord Shift where
    (Shift s1 _ _ _) `compare` (Shift s2 _ _ _) = s1 `compare` s2


data Event = Start Int | Asleep | Awake deriving Show

partOne :: IO [Shift]
partOne = do
  inp <- traverse (C.parse shiftParser) . lines <$> readFile "data/day4.txt"
  pure $ case inp of
    Left  _ -> []
    Right s -> List.sort s

-- partTwo :: IO [Claim]
-- partTwo =
--   filter (\(Claim claimId ints) -> null ints) <$> intersectionsForClaims


shiftParser :: Parser Shift
shiftParser = do
  m <- P.char '[' *> C.numberParser *> P.char '-' *> C.numberParser <* P.char
    '-'
  d   <- C.numberParser <* P.char ' '
  h   <- C.numberParser <* P.char ':'
  min <- C.numberParser <* P.string "] "
  e   <- eventParser
  pure $ Shift
    ((show m) <> "-" <> (show d) <> " " <> (show h) <> ":" <> (show min))
    h
    min
    e


eventParser :: Parser Event
eventParser =
  (Start <$> (P.string "Guard #" *> C.numberParser <* P.string " begins shift"))
    P.<|> (Asleep <$ P.string "falls asleep")
    P.<|> (Awake <$ P.string "wakes up")

