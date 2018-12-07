module Day7 where

import qualified Common                        as C
import qualified Data.Char                     as Char
import qualified Data.List                     as L
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import qualified Text.Parsec                   as P
import           Text.ParserCombinators.Parsec (Parser)

type Dependencies = M.Map Char (S.Set Char)

solution :: IO (String, Int)
solution = do
  inp <- traverse (C.parse instrParser) . lines <$> readFile "data/day7.txt"
  pure $ case inp of
    Left  _ -> undefined
    Right i ->
        ( partOne $ parse i
        , partTwo [] (parse i) 0
        )

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

effort :: Char -> Int
effort c = (Char.ord c - 64) + baseEffort

baseEffort :: Int
baseEffort = 60

maxWorkers :: Int
maxWorkers = 5

partTwo :: [(Char, Int)] -> Dependencies -> Int -> Int
partTwo inprog d n =
    let (ip, comp) = makeProgress inprog
        d' = completeTasks d comp
        avail = filter (notInProg ip) (M.keys $ M.filter S.null d')
        canProcess = take (maxWorkers - length ip) avail
    in
        case (ip, avail) of
            ([], []) -> n
            _ -> partTwo (ip <> ((\c -> (c, effort c)) <$> canProcess)) d' (n+1)
    where
        notInProg :: [(Char, Int)] -> Char -> Bool
        notInProg p c = not $ any (\(c', _) -> c' == c) p

completeTasks :: Dependencies -> String -> Dependencies
completeTasks =
    foldr
        (\c m ->
            M.map (S.delete c) $ M.delete c m
        )

makeProgress :: [(Char, Int)] -> ([(Char, Int)], String)
makeProgress =
    foldr
        (\(c, n) (inprog', complete) ->
            if n > 1
            then ((c, n - 1) : inprog', complete)
            else (inprog', c : complete)
        ) ([], [])
