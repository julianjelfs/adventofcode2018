module Day1
  ( solve
  )
where

import qualified Common                        as C
import           Data.Maybe                     ( catMaybes )
import qualified Data.Set                      as Set
import           Text.Read                      ( readMaybe )

solve :: IO Int
solve = do
  inp <- cycle . catMaybes . fmap readNum . lines <$> readFile "data/day1.txt"
  return $ firstDup 0 Set.empty inp
 where
  readNum :: String -> Maybe Int
  readNum ('+' : xs) = readMaybe xs
  readNum str        = readMaybe str

  firstDup :: Int -> Set.Set Int -> [Int] -> Int
  firstDup freq prev (x : xs)
    | Set.member freq prev = freq
    | otherwise            = firstDup (freq + x) (Set.insert freq prev) xs

