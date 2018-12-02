module Day2 where

import qualified Common                        as C
import qualified Data.Map                      as M
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text

data Counts = Counts Bool Bool deriving (Show)

solve :: IO Int
solve = checksum . totals . fmap countsInWord . Text.lines <$> Text.readFile
  "data/day2.txt"

countsInWord :: Text -> Counts
countsInWord w = M.foldr
  (\v c@(Counts twos threes) -> case v of
    2 -> Counts True threes
    3 -> Counts twos True
    _ -> c
  )
  (Counts False False)
  dict
 where
  dict = Text.foldr
    (\c m -> case c `M.member` m of
      True  -> M.update (\v -> Just (v + 1)) c m
      False -> M.insert c 1 m
    )
    M.empty
    w

totals :: [Counts] -> (Int, Int)
totals = foldr foldCounts (0, 0)
 where
  foldCounts :: Counts -> (Int, Int) -> (Int, Int)
  foldCounts (Counts True  False) (twos, threes) = (twos + 1, threes)
  foldCounts (Counts True  True ) (twos, threes) = (twos + 1, threes + 1)
  foldCounts (Counts False True ) (twos, threes) = (twos, threes + 1)
  foldCounts (Counts False False) (twos, threes) = (twos, threes)

checksum :: (Int, Int) -> Int
checksum (twos, threes) = twos * threes
