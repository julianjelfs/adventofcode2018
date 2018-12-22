{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE ViewPatterns #-}
module Day15 where

import qualified Data.List                     as L
import qualified Data.Map                      as M
import qualified Test.Hspec                    as Test

newtype AttackPower = AttackPower Int deriving (Show, Eq)

newtype HitPoints = HitPoints Int deriving (Show, Eq)

data Combatant
    = Goblin AttackPower HitPoints
    | Elf AttackPower HitPoints
    deriving (Show, Eq)

data Cell
    = Wall
    | Unit Combatant
    | Space
    deriving (Show, Eq)

type Coord = (Int, Int)

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

takeATurn :: Int -> (Coord, Cell) -> Grid -> (Bool, Grid)
takeATurn completeRounds unit@((y, x), Unit combatant) g@(gridToList -> listGrid)
  = case adjacentTargets unit g of
    (t : _) -> undefined -- attack t
    []      -> case targets unit listGrid of
      []      -> (True, g) -- This is game over
      (t : _) -> undefined -- move towards this target
takeATurn _ _ _ = error "non-combatants should not be taking turns"

gridToList :: Grid -> [(Coord, Cell)]
gridToList = M.toList

units :: Grid -> [(Coord, Cell)]
units = L.filter isUnit . gridToList

targets :: (Coord, Cell) -> [(Coord, Cell)] -> [(Coord, Cell)]
targets unit = L.filter (\u -> u /= unit && u `isTargetOf` unit)

isTargetOf :: (Coord, Cell) -> (Coord, Cell) -> Bool
isTargetOf (_, Unit (Goblin _ _)) (_, Unit (Elf _ _)   ) = True
isTargetOf (_, Unit (Elf _ _)   ) (_, Unit (Goblin _ _)) = True
isTargetOf _                      _                      = False

adjacentTargets :: (Coord, Cell) -> Grid -> [(Coord, Cell)]
adjacentTargets c = L.filter (isTargetOf c) . adjacentCells c

isUnit :: (Coord, Cell) -> Bool
isUnit (_, Unit _) = True
isUnit _           = False

isSpace :: (Coord, Cell) -> Bool
isSpace (_, Space) = True
isSpace _          = False

adjacentCells :: (Coord, Cell) -> Grid -> [(Coord, Cell)]
adjacentCells ((y, x), _) grid = foldr
  (\c' cells -> case dolookup c' of
    Nothing  -> cells
    Just adj -> (c', adj) : cells
  )
  []
  coords
 where
  dolookup c = M.lookup c grid
  coords = [(y - 1, x), (y, x - 1), (y, x + 1), (y + 1, x)]

availableAdjacentCells :: (Coord, Cell) -> Grid -> [(Coord, Cell)]
availableAdjacentCells c = L.filter isSpace . adjacentCells c


parseCell :: Char -> Cell
parseCell = \case
  '#' -> Wall
  'G' -> Unit $ Goblin (AttackPower 3) (HitPoints 200)
  'E' -> Unit $ Elf (AttackPower 3) (HitPoints 200)
  '.' -> Space
  _   -> error "parse error"


testGrid1 :: [String]
testGrid1 = ["#######", "#E..G.#", "#...#.#", "#.G.#G#", "#######"]

--honestly feel like we need tests for this one
runTests :: IO ()
runTests = Test.hspec $ Test.describe "Support functions" $ do
  let g = grid testGrid1
  let u = units g

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
    $ case M.lookup (1, 1) g of
        Just c  -> isUnit ((1, 1), c) `Test.shouldBe` True
        Nothing -> undefined

  Test.it "should correctly tell us if a cell is a space"
    $ case M.lookup (1, 2) g of
        Just c  -> isSpace ((1, 2), c) `Test.shouldBe` True
        Nothing -> undefined

  Test.it "should give us available surrounding cells for a cell"
    $ let surrounding = availableAdjacentCells ((3, 2), Space) g
      in  case surrounding of
            [(c1, _), (c2, _), (c3, _)] -> do
              c1 `Test.shouldBe` ((2, 2) :: (Int, Int))
              c2 `Test.shouldBe` ((3, 1) :: (Int, Int))
              c3 `Test.shouldBe` ((3, 3) :: (Int, Int))
            _ -> error "we didn't get the number of results that we expected"

  Test.it "should give ud any adjacent targets in the right order"
    $ let g = grid ["#G#####", "#EG.G.#"]
          e = Unit (Elf (AttackPower 0) (HitPoints 0))
          t = adjacentTargets ((1, 1), e) g
      in  case t of
            [(c1, Unit (Goblin _ _)), (c2, Unit (Goblin _ _))] -> do
              c1 `Test.shouldBe` ((0, 1) :: (Int, Int))
              c2 `Test.shouldBe` ((1, 2) :: (Int, Int))
            _ -> error "we didn't get the number of results that we expected"

  Test.it "should give us any adjacent cells in the right order"
    $ let g = grid ["#G#####", "#EG.G.#"]
          e = Unit (Elf (AttackPower 0) (HitPoints 0))
          t = adjacentCells ((1, 1), e) g
      in  case t of
            [(c1, Unit (Goblin _ _)), (c2, Wall), (c3, Unit (Goblin _ _))] ->
              do
                c1 `Test.shouldBe` ((0, 1) :: (Int, Int))
                c2 `Test.shouldBe` ((1, 0) :: (Int, Int))
                c3 `Test.shouldBe` ((1, 2) :: (Int, Int))
            _ -> error "we didn't get the number of results that we expected"



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

