module Day7 where

import qualified Common                        as C
import qualified Data.List                     as L
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import qualified Text.Parsec                   as P
import           Text.ParserCombinators.Parsec (Parser)

type Dependencies = M.Map Char (S.Set Char)

solution :: IO String
solution = do
  inp <- traverse (C.parse instrParser) . lines <$> readFile "data/day7.txt"
  pure $ case inp of
    Left  _ -> undefined
    Right i -> partOne $ parse i

partOne :: Dependencies -> String
partOne = go
  where
    go d =
      case nextStep d of
        Just (c, _) -> c : go (M.map (S.delete c) $ M.delete c d)
        Nothing     -> []

    nextStep :: Dependencies -> Maybe (Char, S.Set Char)
    nextStep = M.lookupMin . M.filter S.null

parse :: [(Char, Char)] -> Dependencies
parse i =
    foldr (\c m -> M.insert c (dependentSteps c i) m) M.empty (allSteps i)

dependentSteps :: Char -> [(Char, Char)] -> S.Set Char
dependentSteps c i =
    S.fromList . L.sort $ fst <$> filter (\(_, a) -> a == c) i

allSteps :: [(Char, Char)] -> S.Set Char
allSteps = foldr (\(a, b) s -> S.insert a (S.insert b s)) S.empty

instrParser :: Parser (Char, Char)
instrParser = do
  before <- P.string "Step " *> P.anyChar
  after  <- P.string " must be finished before step " *> P.anyChar
  pure (before, after)

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


