module Day9 where

import           Data.Sequence                  ( Seq
                                                , (!?)
                                                )
import qualified Data.Sequence                 as Seq
--import           Debug.Trace                    ( traceShowId )

solution :: ((Seq Int, Seq Int), Int)
solution = (partOne, partTwo)

partOne :: (Seq Int, Seq Int)
partOne = go (Seq.replicate numWorkers 0) (Seq.fromList [0]) 0 1
 where
  go :: Seq Int -> Seq Int -> Int -> Int -> (Seq Int, Seq Int)
  go workers circle ix n
    | n `mod` 23 == 0
    = let (v, ix', circle') = sevenBack circle ix
          wi                = workerIndex n
          currentScore      = workers !? wi
          workers'          = case currentScore of
            Nothing -> workers
            Just s  -> Seq.update wi (s + n + v) workers
      in  if n + v == lastMarble
            then (circle', workers')
            else go workers' circle' ix' (n + 1)
    | otherwise
    = let ix' = nextIndex circle ix
      in  go workers (Seq.insertAt ix' n circle) ix' (n + 1)

workerIndex :: Int -> Int
workerIndex marble = (marble - 1) `mod` numWorkers

partTwo :: Int
partTwo = undefined

numWorkers :: Int
numWorkers = 9

lastMarble :: Int
lastMarble = 32

sevenBack :: Seq Int -> Int -> (Int, Int, Seq Int)
sevenBack s i =
  let i'  = i - 7
      i'' = if i' < 0 then Seq.length s + i' else i'
      v   = s !? i''
  in  case v of
        Nothing -> (0, i, s)
        Just v' -> (v', i'', Seq.deleteAt i'' s)

nextIndex :: Seq Int -> Int -> Int
nextIndex s i = case (i + 2) `mod` Seq.length s of
  0 -> Seq.length s
  n -> n
