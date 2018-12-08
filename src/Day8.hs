module Day8 where

import qualified Common                        as C
import           Data.Maybe                     ( catMaybes )

data Tree = Tree [Int] [Tree] deriving Show

solution :: IO (Int, Int)
solution = do
  inp <- traverse (C.parse C.numberParser) . words <$> readFile "data/day8.txt"
  pure $ case inp of
    Left  err -> error $ show err
    Right i   -> (sumMetaData tree, sumValue tree) where tree = makeTree i

sumMetaData :: Tree -> Int
sumMetaData (Tree m children) = sum m + sum (sumMetaData <$> children)

sumValue :: Tree -> Int
sumValue (Tree m children) = case children of
  [] -> sum m
  _  -> sum (sumValue <$> catMaybes (childAtIndex <$> m))
 where
  childAtIndex :: Int -> Maybe Tree
  childAtIndex n | n <= 0    = Nothing
                 | otherwise = children C.!? (n - 1)

makeTree :: [Int] -> Tree
makeTree inp = snd $ getNode inp
 where
  getNode :: [Int] -> ([Int], Tree)
  getNode (numChildren : numMeta : tail) =
    let (leftOver, children) = getChildren numChildren (tail, [])
    in  (drop numMeta leftOver, Tree (take numMeta leftOver) children)
  getNode _ = error "oh no"

  getChildren :: Int -> ([Int], [Tree]) -> ([Int], [Tree])
  getChildren n (digits, nodes) = if n == length nodes
    then (digits, nodes)
    else
      let (nextBit, nextChild) = getNode digits
      in  getChildren n (nextBit, nodes <> [nextChild])

testInput :: [Int]
testInput = [2, 3, 0, 3, 10, 11, 12, 1, 1, 0, 1, 99, 2, 1, 1, 2]
--           A-------------------------------------------------
--                 B-----------------C----------------
--                                         D-------
