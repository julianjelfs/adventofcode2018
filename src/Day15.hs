{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE ViewPatterns #-}
module Day15 where

import qualified Data.List                     as L
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import qualified Test.Hspec                    as Test

newtype AttackPower = AttackPower Int deriving (Show, Eq, Ord)

newtype HitPoints = HitPoints Int deriving (Show, Eq, Ord)

data Combatant
    = Goblin AttackPower HitPoints
    | Elf AttackPower HitPoints
    deriving (Show, Eq, Ord)

data Cell
    = Wall
    | Unit Combatant
    | Space
    deriving (Show, Eq, Ord)

type Coord = (Int, Int)

type Pair = (Coord, Cell)

type Path = [Pair]

type Grid = M.Map Coord Cell

solution :: IO Grid
solution = do
  g <- grid . lines <$> readFile "data/day15.txt"
  pure g


grid :: [String] -> Grid
grid rows = L.foldl'
  (\m (y, row) ->
    -- we do (y, x) rather than (x, y) so that we get reading order by default
    L.foldl' (\m (x, c) -> M.insert (y, x) (parseCell c) m) m (zip [0 ..] row)
  )
  M.empty
  (zip [0 ..] rows)

takeATurn :: Pair -> Grid -> (Bool, Grid)
takeATurn unit@(_, Unit _combatant) g@(gridToList -> listGrid) =
  case adjacentTargets g unit of
    (_t : _) -> undefined -- attack t
    []       -> case targets unit listGrid of
      [] -> (True, g) -- This is game over
      ts -> case allAvailableAdjacentCells g ts of
        []          -> (False, g) -- there are targets but none of them have adjacent free cells
        targetCells -> (False, move g targetCells unit)
takeATurn _ _ = error "non-combatants should not be taking turns"


move :: Grid -> [Pair] -> Pair -> Grid
move g destinations unit =
  let paths = L.sortBy comparePaths $ shortestPath g unit <$> destinations
  in  case paths of
        ((_, _, _firstStep : _) : _) -> undefined --move to h
        _                            -> error "we didn't find a move to make"


shortestPath :: Grid -> Pair -> Pair -> (Int, Pair, Path)
shortestPath _g _unit _destination = undefined
    -- foreach child create a new path

search :: Path -> S.Set Coord -> Grid -> Pair -> Pair -> Path
search path visited g to from@(c, _) = if to == from
  then path
  else
    let path'    = path <> [from]
        visited' = S.insert c visited
        subPaths = search path' visited' g to <$> nextSteps
    in  shortest subPaths
 where
  shortest :: [Path] -> Path
  shortest = L.minimumBy (\a b -> length a `compare` length b)

  nextSteps :: [Pair]
  nextSteps = L.filter (not . alreadySeen) $ availableAdjacentCells g from

  alreadySeen :: Pair -> Bool
  alreadySeen (c, _) = S.member c visited


comparePaths :: (Int, Pair, Path) -> (Int, Pair, Path) -> Ordering
comparePaths (l, d, p) (l', d', p') = case compare l l' of
  EQ -> case compare d d' of
    EQ -> case (p, p') of
      (h : _, h' : _) -> compare h h'
      _               -> EQ
    od -> od
  ol -> ol


gridToList :: Grid -> [Pair]
gridToList = M.toList


units :: Grid -> [Pair]
units = L.filter isUnit . gridToList


targets :: Pair -> [Pair] -> [Pair]
targets unit = L.filter (\u -> u /= unit && u `isTargetOf` unit)


isTargetOf :: Pair -> Pair -> Bool
isTargetOf (_, Unit (Goblin _ _)) (_, Unit (Elf _ _)   ) = True
isTargetOf (_, Unit (Elf _ _)   ) (_, Unit (Goblin _ _)) = True
isTargetOf _                      _                      = False


adjacentTargets :: Grid -> Pair -> [Pair]
adjacentTargets g c = L.filter (isTargetOf c) $ adjacentCells g c


isUnit :: Pair -> Bool
isUnit (_, Unit _) = True
isUnit _           = False


isSpace :: Pair -> Bool
isSpace (_, Space) = True
isSpace _          = False


adjacentCells :: Grid -> Pair -> [Pair]
adjacentCells grid ((y, x), _) = foldr
  (\c' cells -> case dolookup c' of
    Nothing  -> cells
    Just adj -> (c', adj) : cells
  )
  []
  coords
 where
  dolookup c = M.lookup c grid
  coords = [(y - 1, x), (y, x - 1), (y, x + 1), (y + 1, x)]


allAvailableAdjacentCells :: Grid -> [Pair] -> [Pair]
allAvailableAdjacentCells g targets =
  L.sort . L.nub . concat $ availableAdjacentCells g <$> targets

availableAdjacentCells :: Grid -> Pair -> [Pair]
availableAdjacentCells g = L.filter isSpace . adjacentCells g


parseCell :: Char -> Cell
parseCell = \case
  '#' -> Wall
  'G' -> Unit $ Goblin (AttackPower 3) (HitPoints 200)
  'E' -> Unit $ Elf (AttackPower 3) (HitPoints 200)
  '.' -> Space
  _   -> error "parse error"


testGrid :: Grid
testGrid = grid ["#######", "#E..G.#", "#...#.#", "#.G.#G#", "#######"]

--honestly feel like we need tests for this one
runTests :: IO ()
runTests = Test.hspec $ Test.describe "Day15 tests" $ do
  let u = units testGrid

  Test.it "is possible to get the list of units from the grid"
    $               L.length u
    `Test.shouldBe` (4 :: Int)

  Test.it "units should be in the right order" $ case u of
    [(c1, _), (c2, _), (c3, _), (c4, _)] -> do
      c1 `Test.shouldBe` ((1, 1) :: (Int, Int))
      c2 `Test.shouldBe` ((1, 4) :: (Int, Int))
      c3 `Test.shouldBe` ((3, 2) :: (Int, Int))
      c4 `Test.shouldBe` ((3, 5) :: (Int, Int))
    _ -> undefined

  Test.it "can get targets from units" $ case u of
    (c1 : _) -> L.length (targets c1 u) `Test.shouldBe` (3 :: Int)
    _        -> undefined

  Test.it "targets should only include opposite combatants" $ case u of
    (_ : c1 : _) -> L.length (targets c1 u) `Test.shouldBe` (1 :: Int)
    _            -> undefined

  Test.it "should correctly tell us if a cell is a unit"
    $ case M.lookup (1, 1) testGrid of
        Just c  -> isUnit ((1, 1), c) `Test.shouldBe` True
        Nothing -> undefined

  Test.it "should correctly tell us if a cell is a space"
    $ case M.lookup (1, 2) testGrid of
        Just c  -> isSpace ((1, 2), c) `Test.shouldBe` True
        Nothing -> undefined

  Test.it "should give us available surrounding cells for a cell"
    $ let surrounding = availableAdjacentCells testGrid ((3, 2), Space)
      in  case surrounding of
            [(c1, _), (c2, _), (c3, _)] -> do
              c1 `Test.shouldBe` ((2, 2) :: (Int, Int))
              c2 `Test.shouldBe` ((3, 1) :: (Int, Int))
              c3 `Test.shouldBe` ((3, 3) :: (Int, Int))
            _ -> error "we didn't get the number of results that we expected"

  Test.it "should give ud any adjacent targets in the right order"
    $ let g = grid ["#G#####", "#EG.G.#"]
          e = Unit (Elf (AttackPower 0) (HitPoints 0))
          t = adjacentTargets g ((1, 1), e)
      in  case t of
            [(c1, Unit (Goblin _ _)), (c2, Unit (Goblin _ _))] -> do
              c1 `Test.shouldBe` ((0, 1) :: (Int, Int))
              c2 `Test.shouldBe` ((1, 2) :: (Int, Int))
            _ -> error "we didn't get the number of results that we expected"

  Test.it "should give us any adjacent cells in the right order"
    $ let g = grid ["#G#####", "#EG.G.#"]
          e = Unit (Elf (AttackPower 0) (HitPoints 0))
          t = adjacentCells g ((1, 1), e)
      in  case t of
            [(c1, Unit (Goblin _ _)), (c2, Wall), (c3, Unit (Goblin _ _))] ->
              do
                c1 `Test.shouldBe` ((0, 1) :: (Int, Int))
                c2 `Test.shouldBe` ((1, 0) :: (Int, Int))
                c3 `Test.shouldBe` ((1, 2) :: (Int, Int))
            _ -> error "we didn't get the number of results that we expected"

  Test.it "should give us *all* available adjacent cells given multiple targets"
    $ let
        e     = Unit (Elf (AttackPower 0) (HitPoints 0))
        ts    = targets ((1, 1), e) (gridToList testGrid)
        cells = allAvailableAdjacentCells testGrid ts
      in
        case cells of
          [(c1, Space), (c2, Space), (c3, Space), (c4, Space), (c5, Space), (c6, Space)]
            -> do
              c1 `Test.shouldBe` ((1, 3) :: (Int, Int))
              c2 `Test.shouldBe` ((1, 5) :: (Int, Int))
              c3 `Test.shouldBe` ((2, 2) :: (Int, Int))
              c4 `Test.shouldBe` ((2, 5) :: (Int, Int))
              c5 `Test.shouldBe` ((3, 1) :: (Int, Int))
              c6 `Test.shouldBe` ((3, 3) :: (Int, Int))
          _ -> error "we didn't get the number of results that we expected"

  Test.it "should compare paths correctly by length"
    $ let p1 = (1, ((1, 2), Space), [((1, 2), Space)])
          p2 = (2, ((1, 2), Space), [((1, 2), Space)])
      in  comparePaths p1 p2 `Test.shouldBe` LT

  Test.it "should compare paths correctly by destination order"
    $ let p1 = (2, ((2, 2), Space), [((2, 2), Space)])
          p2 = (2, ((1, 2), Space), [((1, 2), Space)])
      in  comparePaths p1 p2 `Test.shouldBe` GT

  Test.it "should compare paths correctly by first step"
    $ let p1 = (2, ((2, 2), Space), [((1, 2), Space)])
          p2 = (2, ((2, 2), Space), [((1, 5), Space)])
      in  comparePaths p1 p2 `Test.shouldBe` LT

  Test.it "should detect equal paths (though this should not happen)"
    $ let p1 = (2, ((2, 2), Space), [((1, 5), Space)])
          p2 = (2, ((2, 2), Space), [((1, 5), Space)])
      in  comparePaths p1 p2 `Test.shouldBe` EQ


--comparePaths :: (Int, Pair, Path) -> (Int, Pair, Path) -> Ordering

  -- Each turn
  -- =========
  --
  -- units take their turn in the order they appear on the grid (default tuple
  -- sort order)
  --
  -- identify all targets
  -- if there are no targets
  -- then combat ends
  -- else
  --    identify all open squares around each target
  --    if not in range or no open squares
  --    then turn ends
  --    else if in range
  --      then attack
  --      else move
  --
  -- where
  --    move =
  --        finds the target square that it can get to in the fewest moves
  --        if there is a tie use reading order
  --        take a single step towards the chosen target along the shortest path
  --        If there is a tie for shortest path take first step that is first in
  --        reading order
  --        attack!
  --
  --    attack =
  --        list all in range (immediately adjacent) targets
  --        if no targets
  --        then turn ends
  --        else find target with fewest hit points (tie resolved by reading
  --        order)
  --        deal damage according to attack power to reduce hit points.
  --

