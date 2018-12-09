module Day9 where

import qualified Data.List.PointedList.Circular
                                               as P
import           Data.Maybe                     ( fromJust )
import           Data.Sequence                  ( Seq
                                                , (!?)
                                                )
import qualified Data.Sequence                 as Seq
import           Debug.Trace                    ( traceShowId )

solution :: ((P.PointedList Int, Seq Int), Int)
solution = (partOne, partTwo)

partOne :: (P.PointedList Int, Seq Int)
partOne = go (Seq.replicate numWorkers 0) (P.singleton 0) 1
 where
  go :: Seq Int -> P.PointedList Int -> Int -> (P.PointedList Int, Seq Int)
  go workers circle n
    | n `mod` 23 == 0
    = let circle'      = P.moveN (-7) circle
          f            = P._focus circle'
          wi           = workerIndex n
          currentScore = workers !? wi
          workers'     = case currentScore of
            Nothing -> workers
            Just s  -> Seq.update wi (s + n + f) workers
          circle'' = fromJust $ P.deleteRight circle'
      in  if n + f == lastMarble
            then (circle'', workers')
            else go workers' circle'' (n + 1)
    | otherwise
    = let circle' = P.insertLeft n . P.moveN 2 $ circle
      in  go workers (traceShowId circle') (n + 1)

workerIndex :: Int -> Int
workerIndex marble = (marble - 1) `mod` numWorkers

partTwo :: Int
partTwo = undefined

numWorkers :: Int
numWorkers = 9

lastMarble :: Int
lastMarble = 32
