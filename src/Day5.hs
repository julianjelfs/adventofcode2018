{-# LANGUAGE OverloadedStrings #-}

module Day5 where

import qualified Data.Char                     as Char
import qualified Data.List                     as List
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text

data Unit = Unit Text deriving Show

instance Semigroup Unit where
    (Unit "") <> (Unit b) = Unit b
    (Unit a) <> (Unit "") = Unit a
    (Unit a) <> (Unit b) =
        case oppositeCase (Text.last a) (Text.head b) of
            True  -> (Unit $ Text.dropEnd 1 a) <> (Unit $ Text.drop 1 b)
            False -> Unit $ a <> b

instance Monoid Unit where
    mempty = Unit mempty

oppositeCase :: Char -> Char -> Bool
oppositeCase c c' = abs (Char.ord c' - Char.ord c) == 32

partOne :: IO Int
partOne = do
  inp <- Text.readFile "data/day5.txt"
  let (Unit solved) = solve inp
  pure $ (Text.length solved) - 1

partTwo :: IO (Char, Int)
partTwo = do
  inp <- Text.readFile "data/day5.txt"
  pure . List.minimumBy shortest $ mapLengths <$> findShortest inp
 where
  mapLengths (c, Unit u) = (c, Text.length u)
  shortest (_, l) (_, l') = l `compare` l'

solve :: Text -> Unit
solve t = foldMap id allunits
 where
  allunits :: [Unit]
  allunits = Text.foldr (\c units -> Unit (Text.singleton c) : units) [] t

findShortest :: Text -> [(Char, Unit)]
findShortest t = solveWithout t <$> ['a' .. 'z']

solveWithout :: Text -> Char -> (Char, Unit)
solveWithout t c = (c, solve $ Text.filter (notChar c) t)

notChar :: Char -> Char -> Bool
notChar l c = Char.ord l /= Char.ord c + 32 && l /= c

testInput :: Text
testInput = "dabAcCaCBAcCcaDA"
