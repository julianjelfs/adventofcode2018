module Day7 where

-- import qualified Common                        as C
-- import qualified Text.Parsec                   as P
-- import           Text.ParserCombinators.Parsec  ( Parser )

solution :: IO Int
solution = do
  inp <- lines <$> readFile "data/day7.txt"
  pure $ length inp
