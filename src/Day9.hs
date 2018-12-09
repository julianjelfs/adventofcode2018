module Day9 where

import qualified Data.IntMap                   as IM
import           Data.Sequence                  ( Seq )
import qualified Data.Sequence                 as Seq
--import           Debug.Trace                    ( traceShowId )

solution :: (Int, Int)
solution = (partOne, partTwo)

partOne :: Int
partOne = go IM.empty (Seq.fromList [0]) 0 1
 where
  go :: IM.IntMap Int -> Seq Int -> Int -> Int -> Int
  go workers circle ix n
    | n `mod` 23 == 0
    = let ix'      = (ix - 7) `mod` Seq.length circle
          circle'  = Seq.deleteAt ix' circle
          score    = n + Seq.index circle ix'
          wi       = workerIndex n
          workers' = IM.insertWith (+) wi score workers
      in  if score == target
            then maximum workers'
            else go workers' circle' ix' (n + 1)
    | otherwise
    = let ix' = nextIndex circle ix
      in  go workers (Seq.insertAt ix' n circle) ix' (n + 1)

workerIndex :: Int -> Int
workerIndex marble = marble `mod` numWorkers

partTwo :: Int
partTwo = undefined

numWorkers :: Int
numWorkers = 10

target :: Int
target = 1618

nextIndex :: Seq Int -> Int -> Int
nextIndex s i = (i + 1) `mod` Seq.length s + 1