module Day5 where

import qualified Common                        as C
import qualified Data.Set                      as S
import qualified Text.Parsec                   as P
import           Text.ParserCombinators.Parsec (Parser)

partOne :: IO Int
partOne = do
  inp <- lines <$> readFile "data/day5.txt"
  pure inp

