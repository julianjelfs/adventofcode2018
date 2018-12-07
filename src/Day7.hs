module Day7 where

import qualified Common                        as C
import qualified Data.Set                      as S
import qualified Text.Parsec                   as P
import           Text.ParserCombinators.Parsec  ( Parser )

solution :: IO (S.Set Char)
solution = do
  inp <- traverse (C.parse instrParser) . lines <$> readFile "data/day7.txt"
  pure $ case inp of
    Left  _ -> undefined
    Right i -> allSteps i

allSteps :: [(Char, Char)] -> S.Set Char
allSteps = foldr (\(a, b) s -> S.insert a (S.insert b s)) S.empty

removeCompleteStep :: [(Char, Char)] -> Char -> [(Char, Char)]
removeCompleteStep instrs c = filter (\(a, _) -> a /= c) instrs

findAvailableStep :: S.Set Char -> [(Char, Char)] -> S.Set Char
findAvailableStep all instrs = S.difference all (S.fromList $ snd <$> instrs)

instrParser :: Parser (Char, Char)
instrParser = do
  before <- P.string "Step " *> P.anyChar
  after  <- P.string " must be finished before step " *> P.anyChar
  pure $ (before, after)

testInput :: [(Char, Char)]
testInput =
  [ ('C', 'A')
  , ('C', 'F')
  , ('A', 'B')
  , ('A', 'D')
  , ('B', 'E')
  , ('D', 'E')
  , ('F', 'E')
  ]


