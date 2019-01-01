{-# LANGUAGE ScopedTypeVariables #-}
module Day15Tests where

import           Day15

import qualified Data.List                     as L
import qualified Data.Map                      as M
import qualified Test.Hspec                    as Test

testGrid :: Grid
testGrid = grid 3 ["#######", "#E..G.#", "#...#.#", "#.G.#G#", "#######"]

runTests :: IO ()
runTests =
  Test.hspec
    $ Test.describe "Day15 tests"
    $ do
        let u      = units testGrid
            elf    = Unit (Elf (AttackPower 3) (HitPoints 200))
            goblin = Unit (Goblin (AttackPower 3) (HitPoints 200))

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
                  _ -> error
                    "we didn't get the number of results that we expected"

        Test.it "should give ud any adjacent targets in the right order"
          $ let g = grid 3 ["#G#####", "#EG.G.#"]
                e = Unit (Elf (AttackPower 0) (HitPoints 0))
                t = adjacentTargets g ((1, 1), e)
            in  case t of
                  [(c1, Unit (Goblin _ _)), (c2, Unit (Goblin _ _))] -> do
                    c1 `Test.shouldBe` ((0, 1) :: (Int, Int))
                    c2 `Test.shouldBe` ((1, 2) :: (Int, Int))
                  _ -> error
                    "we didn't get the number of results that we expected"

        Test.it "should give us any adjacent cells in the right order"
          $ let g = grid 3 ["#G#####", "#EG.G.#"]
                t = adjacentCells g ((1, 1), elf)
            in  case t of
                  [(c1, Unit (Goblin _ _)), (c2, Wall), (c3, Unit (Goblin _ _))]
                    -> do
                      c1 `Test.shouldBe` ((0, 1) :: (Int, Int))
                      c2 `Test.shouldBe` ((1, 0) :: (Int, Int))
                      c3 `Test.shouldBe` ((1, 2) :: (Int, Int))
                  _ -> error
                    "we didn't get the number of results that we expected"

        Test.it
            "should give us *all* available adjacent cells given multiple targets"
          $ let
              ts    = targets ((1, 1), elf) (gridToList testGrid)
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
                _ ->
                  error "we didn't get the number of results that we expected"

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

        Test.it "can move a unit and leave the grid in the right state"
          $ let g    = grid 3 ["E.", ".G"]
                from = ((0, 0), elf)
                to   = ((0, 1), Space)
                g'   = move False g from to
            in  do
                  M.lookup (0, 1) g' `Test.shouldBe` Just elf
                  M.lookup (0, 0) g' `Test.shouldBe` Just Space

        -- this test fails at the moment
        Test.describe "Whole simulation" $ do

          Test.it "test grid one"
            $ let g = grid
                    3
                    [ "#######"
                    , "#.G...#"
                    , "#...EG#"
                    , "#.#.#G#"
                    , "#..G#E#"
                    , "#.....#"
                    , "#######"
                    ]
              in  do
                    res <- playGame False 0 g
                    res `Test.shouldBe` (47, 590, 0, 4)

          Test.it "test grid two"
            $ let g = grid
                    3
                    [ "#######"
                    , "#G..#E#"
                    , "#E#E.E#"
                    , "#G.##.#"
                    , "#...#E#"
                    , "#...E.#"
                    , "#######"
                    ]
              in  do
                    res <- playGame False 0 g
                    res `Test.shouldBe` (37, 982, 5, 0)

          Test.it "test grid three"
            $ let g = grid
                    3
                    [ "#######"
                    , "#E..EG#"
                    , "#.#G.E#"
                    , "#E.##E#"
                    , "#G..#.#"
                    , "#..E#.#"
                    , "#######"
                    ]
              in  do
                    res <- playGame False 0 g
                    res `Test.shouldBe` (46, 859, 5, 0)

          Test.it "test grid four"
            $ let g = grid
                    3
                    [ "#######"
                    , "#E.G#.#"
                    , "#.#G..#"
                    , "#G.#.G#"
                    , "#G..#.#"
                    , "#...E.#"
                    , "#######"
                    ]
              in  do
                    res <- playGame False 0 g
                    res `Test.shouldBe` (35, 793, 0, 5)

          Test.it "test grid five"
            $ let g = grid
                    3
                    [ "#######"
                    , "#.E...#"
                    , "#.#..G#"
                    , "#.###.#"
                    , "#E#G#G#"
                    , "#...#G#"
                    , "#######"
                    ]
              in  do
                    res <- playGame False 0 g
                    res `Test.shouldBe` (54, 536, 0, 4)

          Test.it "test grid six"
            $ let g = grid
                    3
                    [ "#########"
                    , "#G......#"
                    , "#.E.#...#"
                    , "#..##..G#"
                    , "#...##..#"
                    , "#...#...#"
                    , "#.G...G.#"
                    , "#.....G.#"
                    , "#########"
                    ]
              in  do
                    res <- playGame False 0 g
                    res `Test.shouldBe` (20, 937, 0, 5)

        Test.describe "Path finding" $ do
          let g  = grid 3 ["E.", ".G"]
              to = ((1, 1), goblin)

          Test.it "should find the correct next steps"
            $ let from  = ((0, 1), Space)
                  cells = validAdjacentCells g to from
              in  case cells of
                    [x] -> x `Test.shouldBe` to
                    _   -> error "wrong number of next steps returned"

          Test.it "should find multiple next steps"
            $ let from  = ((0, 0), elf)
                  cells = validAdjacentCells g to from
              in  case cells of
                    [x, y] -> do
                      x `Test.shouldBe` ((0, 1), Space)
                      y `Test.shouldBe` ((1, 0), Space)
                    _ -> error "wrong number of next steps returned"

        Test.describe "Attacking" $ do

          Test.it "should correctly identify the weakest target"
            $ let g1 = ((1, 1), Unit $ Goblin (AttackPower 3) (HitPoints 100))
                  g2 = ((1, 1), Unit $ Goblin (AttackPower 3) (HitPoints 50))
                  g3 = ((1, 1), Unit $ Goblin (AttackPower 3) (HitPoints 25))
                  wk = weakest [g1, g2, g3]
              in  wk `Test.shouldBe` g3

          Test.it "equal weakest should be in reading order"
            $ let g1 = ((1, 5), Unit $ Goblin (AttackPower 3) (HitPoints 100))
                  g2 = ((1, 4), Unit $ Goblin (AttackPower 3) (HitPoints 100))
                  g3 = ((1, 1), Unit $ Goblin (AttackPower 3) (HitPoints 150))
                  wk = weakest [g1, g2, g3]
              in  wk `Test.shouldBe` g2

          Test.it "reduce hit points should work when non-fatal"
            $ let g1 = ((1, 5), Unit $ Goblin (AttackPower 3) (HitPoints 100))
                  g2 = ((1, 5), Unit $ Goblin (AttackPower 3) (HitPoints 97))
              in  reduceHitPointsBy 3 g1 `Test.shouldBe` (g2, False)

          Test.it "reduce hit points should work when fatal"
            $ let g1 = ((1, 5), Unit $ Goblin (AttackPower 3) (HitPoints 3))
                  g2 = ((1, 5), Space)
              in  reduceHitPointsBy 3 g1 `Test.shouldBe` (g2, False)


        Test.describe "A slightly larger example grid" $ do
          let g =
                grid 3 ["#######", "#.E...#", "#.....#", "#...G.#", "#######"]
              from    = ((1, 2), elf)
              to      = ((3, 4), goblin)
              targets = [((3, 3), Space), ((3, 5), Space), ((2, 4), Space)]

          Test.it "should select and make the correct first move"
            $ let g' = findMove False g targets from
              in  do
                    M.lookup (1, 3) g' `Test.shouldBe` Just elf
                    M.lookup (1, 2) g' `Test.shouldBe` Just Space

          Test.it "should select and make the correct second move"
            $ let g'  = findMove False g targets from
                  g'' = findMove False g' targets ((1, 3), elf)
              in  do
                    M.lookup (1, 4) g'' `Test.shouldBe` Just elf
                    M.lookup (1, 3) g'' `Test.shouldBe` Just Space

          Test.it "should find the correct shortest path"
            $ let p = shortestPath g from to
              in  case p of
                    Just (l, d, path) -> do
                      l `Test.shouldBe` (4 :: Int)
                      d `Test.shouldBe` to
                      case path of
                        [one, two, three, four] -> do
                          one `Test.shouldBe` ((1, 3), Space)
                          two `Test.shouldBe` ((1, 4), Space)
                          three `Test.shouldBe` ((2, 4), Space)
                          four `Test.shouldBe` to
                        _ -> error "We should have got a four part path"
                    Nothing -> error "we expected to get a shortest path"

          Test.it "should find all paths correctly"
            $ let paths = allPaths g from to
              in  length paths `Test.shouldBe` (1 :: Int)

          Test.it "should find the correct adjacent cells"
            $ let paths = validAdjacentCells g to ((2, 2), Space)
              in  length paths `Test.shouldBe` (3 :: Int)


          Test.it "should find the correct shortest path via dijkstra"
            $ let distances = minimumDistanceMap g (1, 2)
              in  do
                    length distances `Test.shouldBe` 14
                    M.lookup (3, 3) distances `Test.shouldBe` Just 3

          Test.it "should correctly find the best target"
            $ let t = findBestTarget g from targets
              in  t `Test.shouldBe` Just (2, 4)

          Test.it "should end in the correct state after taking a turn"
            $ let (complete, g') = takeATurn False from g
              in  do
                    complete `Test.shouldBe` False
                    M.lookup (1, 3) g' `Test.shouldBe` Just elf
                    M.lookup (1, 2) g' `Test.shouldBe` Just Space

        Test.describe "With the full grid" $ do
          let g    = grid 3 fullGrid
              from = ((4, 16), goblin)

          Test.it "should find the correct shortest path via dijkstra"
            $ let distances = minimumDistanceMap g (4, 16)
              in  do
                    length distances `Test.shouldBe` 362
                    M.lookup (1, 10) distances `Test.shouldBe` Just 11

          Test.it "should successfully find adjacent targets"
            $               adjacentTargets g from
            `Test.shouldBe` []

          Test.it "should successfully find opponents"
            $ let ts = targets from (gridToList g)
              in  length ts `Test.shouldBe` 10

          Test.it "should successfully find target cells"
            $ let ts    = targets from (gridToList g)
                  cells = allAvailableAdjacentCells g ts
              in  length cells `Test.shouldBe` 34

          Test.it "should successfully find the best target cell"
            $ let ts         = targets from (gridToList g)
                  cells      = allAvailableAdjacentCells g ts
                  bestTarget = findBestTarget g from cells
              in  bestTarget `Test.shouldBe` Just (11, 23)

          Test.it
              "should successfully find the shortest path to the best target cell"
            $ let ts         = targets from (gridToList g)
                  cells      = allAvailableAdjacentCells g ts
                  bestTarget = findBestTarget g from cells
                  res        = case bestTarget of
                    Nothing   -> error "this should not happen"
                    Just cell -> shortestPath g from (cell, Space)
              in  case res of
                    Just (l, _, _) -> l `Test.shouldBe` (14 :: Int)
                    Nothing        -> error "this should not happen"

          Test.it "should successfully run a move"
            $ let (complete, _g') = takeATurn False from g
              in  complete `Test.shouldBe` False

fullGrid :: [String]
fullGrid =
  [ "################################"
  , "##########.###.###..############"
  , "##########..##......############"
  , "#########...##....##############"
  , "######.....###..G..G############"
  , "##########..........############"
  , "##########.............#########"
  , "#######G..#.G...#......#########"
  , "#..G##....##..#.G#....#...######"
  , "##......###..##..####.#..#######"
  , "#G.G..#..#....#.###...G..#######"
  , "#.....GG##................######"
  , "#....G........#####....E.E.#####"
  , "#####G...#...#######........####"
  , "####.E#.G...#########.......####"
  , "#...G.....#.#########......#####"
  , "#.##........#########.......####"
  , "######......#########........###"
  , "######......#########..E.#....##"
  , "#######..E.G.#######..........##"
  , "#######E......#####............#"
  , "#######...G............E.......#"
  , "####............##............##"
  , "####..G.........##..........E.##"
  , "####.G.G#.....####E...##....#.##"
  , "#######.......####...####..#####"
  , "########....E....########..#####"
  , "##########.......#########...###"
  , "##########.......#########..####"
  , "##########....############..####"
  , "###########...##################"
  , "################################"
  ]
