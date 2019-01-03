{-# LANGUAGE ViewPatterns #-}

module Day16 where

import qualified Common                        as C
import           Data.Array                     ( (!)
                                                , (//)
                                                )
import qualified Data.Array                    as A
import           Data.Bits                      ( (.&.)
                                                , (.|.)
                                                )
import qualified Data.List                     as L
import qualified Data.Map                      as M
import qualified Text.Parsec                   as P
import           Text.ParserCombinators.Parsec  ( Parser )

type Register = A.Array Int Int

type Processor = Register -> Instr -> Register

register :: A.Array Int Int
register = A.listArray (0, 3) [0, 0, 0, 0]

allOperations :: [Processor]
allOperations =
  [ addr
  , addi
  , mulr
  , muli
  , banr
  , bani
  , borr
  , bori
  , setr
  , seti
  , gtir
  , gtri
  , gtrr
  , eqir
  , eqri
  , eqrr
  ]

opsMap :: M.Map Int Processor
opsMap = M.fromList $ zip [0 ..] allOperations


addr, addi, mulr, muli, banr, bani, borr, bori, setr, seti, gtir, gtri, gtrr, eqir, eqri, eqrr
  :: Processor
addr = binopr (+)
addi = binopi (+)
mulr = binopr (*)
muli = binopi (*)
banr = binopr (.&.)
bani = binopi (.&.)
borr = binopr (.|.)
bori = binopi (.|.)
setr r (Instr _ a _ c) = r // [(c, r ! a)]
seti r (Instr _ a _ c) = r // [(c, a)]
gtir r (Instr _ a b c) | a > (r ! b) = r // [(c, 1)]
                       | otherwise   = r // [(c, 0)]
gtri r (Instr _ a b c) | (r ! a) > b = r // [(c, 1)]
                       | otherwise   = r // [(c, 0)]
gtrr r (Instr _ a b c) | (r ! a) > (r ! b) = r // [(c, 1)]
                       | otherwise         = r // [(c, 0)]
eqir r (Instr _ a b c) | a == (r ! b) = r // [(c, 1)]
                       | otherwise    = r // [(c, 0)]
eqri r (Instr _ a b c) | (r ! a) == b = r // [(c, 1)]
                       | otherwise    = r // [(c, 0)]
eqrr r (Instr _ a b c) | (r ! a) == (r ! b) = r // [(c, 1)]
                       | otherwise          = r // [(c, 0)]

binopr :: (Int -> Int -> Int) -> Register -> Instr -> Register
binopr op r (Instr _ a b c) = r // [(c, (r ! a) `op` (r ! b))]

binopi :: (Int -> Int -> Int) -> Register -> Instr -> Register
binopi op r (Instr _ a b c) = r // [(c, (r ! a) `op` b)]

data Instr = Instr Int Int Int Int deriving Show

data SampleInstr = SampleInstr Register Instr Register deriving Show

partOne :: IO Int
partOne = do
  samples <- parseSamples
  pure $ case samples of
    Left err -> error $ show err
    Right samples' ->
      length
        $   filter (>= 3)
        $   (\(SampleInstr r1 i r2) ->
              length $ filter id $ (\op -> r2 == op r1 i) <$> allOperations
            )
        <$> samples'

partTwo :: IO [Int]
partTwo = do
  samples <- parseSamples
  instrs  <- parseInstrs
  pure $ case (samples, instrs) of
    (Left err      , _             ) -> error $ show err
    (_             , Left err      ) -> error $ show err
    (Right samples', Right _instrs') -> M.keys $ decode M.empty samples' opsMap


decode
  :: M.Map Int Processor
  -> [SampleInstr]
  -> M.Map Int Processor
  -> M.Map Int Processor
decode decoded _ (M.null -> True) = decoded
decode decoded samples encoded =
  let (d, e) = L.foldr
        (\(SampleInstr r1 i@(Instr opcode _ _ _) r2) (decoded', encoded') ->
          case
              filter fourth
              $   (\(k, op) -> (opcode, op, k, r2 == op r1 i))
              <$> M.toList encoded'
            of
              [(code, op, k, _)] ->
                ( M.insert code op decoded'
                , M.filterWithKey (\k' _ -> k /= k') encoded'
                )
              _ -> (decoded', encoded')
        )
        (decoded, encoded)
        samples
  in  decode d samples e

fourth :: (Int, Processor, Int, Bool) -> Bool
fourth (_, _, _, a) = a


parseInstrs :: IO (Either P.ParseError [Instr])
parseInstrs =
  traverse (C.parse parseInstr) . lines <$> readFile "data/day16_2.txt"

parseSamples :: IO (Either P.ParseError [SampleInstr])
parseSamples =
  C.parse (P.manyTill parseSample P.eof) <$> readFile "data/day16_1.txt"

parseInstr :: Parser Instr
parseInstr =
  Instr
    <$> (C.numberParser <* P.char ' ')
    <*> (C.numberParser <* P.char ' ')
    <*> (C.numberParser <* P.char ' ')
    <*> C.numberParser

parseRegister :: String -> Parser Register
parseRegister prefix = do
  _         <- P.string prefix
  registers <- P.char '[' *> P.sepBy C.numberParser (P.string ", ") <* P.char
    ']'
  pure $ A.listArray (0, 3) registers

parseSample :: Parser SampleInstr
parseSample = do
  before <- parseRegister "Before: " <* P.endOfLine
  instr  <- parseInstr <* P.endOfLine
  after  <- parseRegister "After:  " <* P.endOfLine
  _      <- P.endOfLine
  pure $ SampleInstr before instr after


testParseInstr :: String -> Either P.ParseError Instr
testParseInstr = C.parse parseInstr
