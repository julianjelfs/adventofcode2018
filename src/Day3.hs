module Day3 where

import qualified Common                        as C
import qualified Data.Set                      as S
import qualified Text.Parsec                   as P
import           Text.ParserCombinators.Parsec (Parser)

data Claim = Claim Int (S.Set (Int, Int)) deriving Show

instance Semigroup Claim where
    (Claim _ a) <> (Claim _ b) = Claim 0 (a <> b)

instance Monoid Claim where
    mempty = Claim 0 S.empty

partOne :: IO Int
partOne = do
  (Claim _ s) <- foldMap id <$> intersectionsForClaims
  pure $ length s

partTwo :: IO [Claim]
partTwo = filter (\(Claim claimId ints) -> null ints) <$> intersectionsForClaims

intersectionsForClaims :: IO [Claim]
intersectionsForClaims = do
  claims <- traverse (C.parse claimParser) . lines <$> readFile "data/day3.txt"
  pure $ case claims of
    Left  err -> []
    Right c   ->
        intersectionsForClaim c <$> c

intersectionsForClaim :: [Claim] -> Claim -> Claim
intersectionsForClaim all (Claim claimId coords) =
    Claim claimId . foldMap id $
        (\(Claim claimId' coords') ->
            if claimId /= claimId'
            then coords `S.intersection` coords'
            else S.empty
        ) <$> all

claimParser :: Parser Claim
claimParser = do
    claimId <- P.char '#' *> C.numberParser
    x <- P.string " @ " *> C.numberParser
    y <- P.char ',' *> C.numberParser
    w <- P.string ": " *> C.numberParser
    h <- P.char 'x' *> C.numberParser
    pure $ Claim claimId (S.fromList [ (a, b) | a <- [x .. (x + (w-1))], b <- [y .. (y + (h-1))] ])
