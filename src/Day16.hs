module Day16 where

import qualified Data.Array                    as A

register :: A.Array Int Int
register = A.array (0, 3) [0, 0, 0, 0]

data Instr
    = Addr Char Char
    | Addi Char Int
    | Mulr Char Char
    | Muli Char Int
    | Banr Char Char
    | Bani Char Int
    | Borr Char Char
    | Bori Char Int
    | Setr Char Char
    | Seti Char Int
    | Gtir Int Char
    | Gtri Char Int
    | Gtrr Char Char
    | Etir Int Char
    | Etri Char Int
    | Etrr Char Char
    deriving (Show)


solve :: IO Int
solve = fmap parseInstr . lines <$> readFile "data/day16.txt" >>= partOne

partOne :: [Instr] -> Int
partOne instrs = undefined

parseInstr :: String -> Instr
parseInstr = undefined
