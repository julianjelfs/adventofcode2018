module Day6 where

-- import qualified Common                        as C
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
-- import qualified Text.Parsec                   as P
-- import           Text.ParserCombinators.Parsec  ( Parser )

partOne :: IO [Text]
partOne = do
  inp <- Text.lines <$> Text.readFile "data/day4.txt"
  pure inp
