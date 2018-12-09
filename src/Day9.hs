module Day9 where

import qualified Data.IntMap                   as IM
import           Data.List                      ( foldl' )
import           Data.Sequence                  ( Seq )
import qualified Data.Sequence                 as Seq

data State = State
    { scores :: IM.IntMap Int
    , index  :: Int
    , circle :: Seq Int
    } deriving Show

solution :: (Int, Int)
solution =
  (maximum . scores $ solve 72061, maximum . scores $ solve (72061 * 100))

solve :: Int -> State
solve target = foldl' go (State IM.empty 0 (Seq.fromList [0])) [1 .. target]
 where
  go :: State -> Int -> State
  go (State scores index circle) n
    | n `mod` 23 == 0
    = let index'  = moveBack circle index
          circle' = Seq.deleteAt index' circle
          score   = n + Seq.index circle index'
          scores' = IM.insertWith (+) (workerIndex n) score scores
      in  State scores' (adjustIndex circle index') circle'
    | otherwise
    = let index' = nextIndex circle index
      in  State scores index' (Seq.insertAt index' n circle)

adjustIndex :: Seq Int -> Int -> Int
adjustIndex s i = if Seq.length s - 1 == i then 0 else i

workerIndex :: Int -> Int
workerIndex marble = marble `mod` numWorkers

numWorkers :: Int
numWorkers = 428

nextIndex :: Seq Int -> Int -> Int
nextIndex s i = (i + 1) `mod` Seq.length s + 1

moveBack :: Seq Int -> Int -> Int
moveBack s i = (i - 7) `mod` Seq.length s
