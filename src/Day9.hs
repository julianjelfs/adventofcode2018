module Day9 where

import qualified Data.IntMap                   as IM
import           Data.Sequence                  ( Seq )
import qualified Data.Sequence                 as Seq
-- import           Debug.Trace                    ( traceShowId )

solution :: ((Seq Int, Int), Int)
solution = (partOne, partTwo)

partOne :: (Seq Int, Int)
partOne = go IM.empty (Seq.fromList [0]) 0 1
 where
  go :: IM.IntMap Int -> Seq Int -> Int -> Int -> (Seq Int, Int)
  go workers circle ix n
    | n `mod` 23 == 0
    = let ix'      = moveBack circle ix
          circle'  = Seq.deleteAt ix' circle
          score    = n + Seq.index circle ix'
          workers' = IM.insertWith (+) (workerIndex n) score workers
      in  if score == target
            then (circle', maximum workers')
            else go workers' circle' (adjustIndex circle ix') (n + 1)
    | otherwise
    = let ix' = nextIndex circle ix
      in  go workers (Seq.insertAt ix' n circle) ix' (n + 1)

adjustIndex :: Seq Int -> Int -> Int
adjustIndex s i = if i == length s - 1 then 0 else i

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

moveBack :: Seq Int -> Int -> Int
moveBack s i = (i - 7) `mod` Seq.length s
