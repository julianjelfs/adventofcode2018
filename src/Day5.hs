{-# LANGUAGE OverloadedStrings #-}

module Day5 where

import qualified Data.Char     as Char
import           Data.Foldable (fold)
import           Data.Text     (Text)
import qualified Data.Text     as Text
import qualified Data.Text.IO  as Text

newtype Unit = Unit Text deriving Show

instance Semigroup Unit where
    (Unit "") <> (Unit b) = Unit b
    (Unit a) <> (Unit "") = Unit a
    (Unit a) <> (Unit b) =
        if oppositeCase (Text.last a) (Text.head b)
        then (Unit $ Text.dropEnd 1 a) <> (Unit $ Text.drop 1 b)
        else Unit $ a <> b

instance Monoid Unit where
    mempty = Unit mempty

oppositeCase :: Char -> Char -> Bool
oppositeCase c c' = abs (Char.ord c' - Char.ord c) == 32

solution :: IO (Int, Int)
solution = do
  inp <- Text.dropEnd 1 <$> Text.readFile "data/day5.txt"
  let (Unit solved) = reactPolymer inp
  pure (Text.length solved, minimum $ mapLengths <$> findShortest inp)
 where
  mapLengths (Unit u) = Text.length u

reactPolymer :: Text -> Unit
reactPolymer t = fold allunits
 where
  allunits :: [Unit]
  allunits = Text.foldr (\c units -> Unit (Text.singleton c) : units) [] t

findShortest :: Text -> [Unit]
findShortest t = solveWithout t <$> ['a' .. 'z']
 where
  solveWithout :: Text -> Char -> Unit
  solveWithout t c = reactPolymer $ Text.filter (notChar c) t

  notChar :: Char -> Char -> Bool
  notChar l c = Char.ord l /= Char.ord c + 32 && l /= c
