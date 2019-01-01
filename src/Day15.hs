{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
module Day15 where

import qualified Data.List                     as L
import qualified Data.Map.Strict               as M
import qualified Data.Set                      as S
--import           Debug.Trace                    ( traceShowId )
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

solution :: IO (Int, Int)
solution = grid . lines <$> readFile "data/day15.txt" >>= playGame 0


grid :: [String] -> Grid
grid rows = L.foldl'
  (\m (y, row) ->
    -- we do (y, x) rather than (x, y) so that we get reading order by default
    L.foldl' (\m (x, c) -> M.insert (y, x) (parseCell c) m) m (zip [0 ..] row)
  )
  M.empty
  (zip [0 ..] rows)

playGame :: Int -> Grid -> IO (Int, Int)
playGame rounds g = runARound g >>= \case
  (False, g') -> playGame (rounds + 1) g'
  (True , g') -> pure (rounds, sumHitPoints g')

sumHitPoints :: Grid -> Int
sumHitPoints = M.foldr
  (\cell total -> case cell of
    Unit (Goblin _ (HitPoints h)) -> total + h
    Unit (Elf    _ (HitPoints h)) -> total + h
    _                             -> total
  )
  0

runARound :: Grid -> IO (Bool, Grid)
runARound g = pure $ L.foldl'
  (\(complete, g') u -> if complete then (complete, g') else takeATurn u g')
  (False, g)
  (units g)

takeATurn :: Pair -> Grid -> (Bool, Grid)
takeATurn (uc, _) g@(gridToList -> listGrid) = case M.lookup uc g of
  Nothing    -> (False, g)
  Just Wall  -> (False, g)
  Just Space -> (False, g)
  Just u ->
    let unit = (uc, u)
    in  case adjacentTargets g unit of
          [] -> case targets unit listGrid of
            [] -> (True, g) -- This is game over
            ts -> case allAvailableAdjacentCells g ts of
              []          -> (False, g) -- there are targets but none of them have adjacent free cells
              targetCells -> (False, findMove g targetCells unit)
          adj -> attack unit adj g

attack :: Pair -> [Pair] -> Grid -> (Bool, Grid)
attack unit (weakest -> w@(wc, _)) g =
  let (_, attacked) = reduceHitPointsBy (extractAttackPower unit) w
  in  (False, M.update (\_ -> Just attacked) wc g)

reduceHitPointsBy :: Int -> Pair -> Pair
reduceHitPointsBy n (c, Unit (Goblin ap (HitPoints x)))
  | x - n <= 0 = (c, Space)
  | otherwise  = (c, Unit (Goblin ap (HitPoints (x - n))))
reduceHitPointsBy n (c, Unit (Elf ap (HitPoints x)))
  | x - n <= 0 = (c, Space)
  | otherwise  = (c, Unit (Elf ap (HitPoints (x - n))))
reduceHitPointsBy _ p = p

extractAttackPower :: Pair -> Int
extractAttackPower (_, Unit (Goblin (AttackPower n) _)) = n
extractAttackPower (_, Unit (Elf (AttackPower n) _)) = n
extractAttackPower _ = 0

extractHitPoints :: Pair -> Int
extractHitPoints (_, Unit (Goblin _ (HitPoints n))) = n
extractHitPoints (_, Unit (Elf _ (HitPoints n))) = n
extractHitPoints _ = 0

weakest :: [Pair] -> Pair
weakest []    = error "can't find the weakest in an empty list"
weakest units = L.minimumBy compareCombatants units

compareCombatants :: Pair -> Pair -> Ordering
compareCombatants (p1, Unit (Goblin _ (HitPoints a))) (p2, Unit (Goblin _ (HitPoints b)))
  | a == b
  = p1 `compare` p2
  | otherwise
  = a `compare` b
compareCombatants (p1, Unit (Elf _ (HitPoints a))) (p2, Unit (Elf _ (HitPoints b)))
  | a == b
  = p1 `compare` p2
  | otherwise
  = a `compare` b
compareCombatants _ _ = EQ

findBestTarget :: Grid -> Pair -> [Pair] -> Maybe Coord
findBestTarget g (from, _) possibleTargets =
  let distance = minimumDistanceMap g from
      targetDistances =
        (\(p, _) -> (p, M.lookup p distance)) <$> possibleTargets
  in  case L.sortBy compareDistance $ catMaybes' targetDistances of
        []           -> Nothing
        ((t, _) : _) -> Just t
 where
  catMaybes' :: [(a, Maybe Int)] -> [(a, Int)]
  catMaybes' = foldr
    (\(a, b) xs -> case b of
      Just b' -> (a, b') : xs
      Nothing -> xs
    )
    []

  compareDistance :: (Coord, Int) -> (Coord, Int) -> Ordering
  compareDistance (c, d) (c', d') | d == d'   = c `compare` c'
                                  | otherwise = d `compare` d'


findMove :: Grid -> [Pair] -> Pair -> Grid
findMove g possibleTargets unit = case findBestTarget g unit possibleTargets of
  Nothing -> g
  Just t  -> case shortestPath g unit (t, Space) of
    Nothing        -> g
    Just (_, _, p) -> case p of
      (to : _) -> move g unit to
      _        -> g

move :: Grid -> Pair -> Pair -> Grid
move g (k, v) (k', _) =
  let newUnit  = (k', v)
      deleted  = M.delete k' $ M.delete k g
      inserted = M.insert k Space $ M.insert k' v deleted
  in  case adjacentTargets inserted newUnit of
        []  -> inserted
        adj -> snd $ attack newUnit adj inserted

allPaths :: Grid -> Pair -> Pair -> [Path]
allPaths g from to = go [] S.empty [[from]]
 where
  go :: [Path] -> S.Set Coord -> [Path] -> [Path]
  go paths _ [] = drop 1 . reverse <$> paths -- empty queue
  go paths visited (h : queue)
    = let
        v@(vc, _) = head h
        visited'  = S.insert vc visited
      in
        if v == to
          then let paths' = h : paths in go paths' visited' queue
          else if shortestPath paths < length h
            then drop 1 . reverse <$> paths
            else
              let
                va = L.filter (not . alreadySeen visited')
                  $ validAdjacentCells g to v
                queue' = queue <> ((: h) <$> va)
              in
                go paths (visitNeighbours va visited') queue'

  visitNeighbours :: [Pair] -> S.Set Coord -> S.Set Coord
  visitNeighbours adj v = L.foldr (\(ac, _) v' -> S.insert ac v') v adj

  shortestPath :: [Path] -> Int
  shortestPath []    = maxBound
  shortestPath paths = minimum $ length <$> paths


shortestPath :: Grid -> Pair -> Pair -> Maybe (Int, Pair, Path)
shortestPath g from to =
  let paths  = allPaths g from to
      mapped = (\p -> (length p, to, p)) <$> paths
  in  case L.sortBy comparePaths mapped of
        (shortest : _) -> Just shortest
        []             -> Nothing

--
-- this is a Dijkstra algorithm to find the minimum distance to each space in
-- the map from where we currently are
minimumDistanceMap :: Grid -> Coord -> M.Map Coord Int
minimumDistanceMap g f =
  let distances = M.foldrWithKey
        (\k v dm -> case v of
          Space -> M.insert k maxBound dm
          _     -> dm
        )
        (M.singleton f (0 :: Int))
        g
  in  filterOutUnreachables $ go S.empty distances
 where
  filterOutUnreachables :: M.Map Coord Int -> M.Map Coord Int
  filterOutUnreachables = M.filter (< maxBound)

  go :: S.Set Coord -> M.Map Coord Int -> M.Map Coord Int
  go visited' distances' = case minDistance visited' distances' of
    Nothing     -> distances'
    Just (k, v) -> if v == maxBound   --this means that we have resolved as much as we can - the remaining nodes are unreachable
      then distances'
      else
        let
          visited''   = S.insert k visited'
          ac          = availableAdjacentCells g (k, Space)
          distances'' = L.foldr
            (\(c, _) dm -> case M.lookup c dm of
              Nothing -> dm
              Just d ->
                if v + 1 < d then M.update (\_ -> Just $ v + 1) c dm else dm
            )
            distances'
            ac
        in
          go visited'' distances''

  minDistance :: S.Set Coord -> M.Map Coord Int -> Maybe (Coord, Int)
  minDistance set' = M.foldrWithKey
    (\k v m -> if S.member k set'
      then m
      else case m of
        Nothing       -> Just (k, v)
        Just (k', v') -> Just $ if v' < v then (k', v') else (k, v)
    )
    Nothing



validAdjacentCells :: Grid -> Pair -> Pair -> [Pair]
validAdjacentCells g to from =
  L.filter (validNextStep to) $ adjacentCells g from

validNextStep :: Pair -> Pair -> Bool
validNextStep to p = p == to || isSpace p

alreadySeen :: S.Set Coord -> Pair -> Bool
alreadySeen visited (c, _) = S.member c visited

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


isUnit, isSpace, isWall :: Pair -> Bool
isUnit (_, Unit _) = True
isUnit _           = False

isSpace (_, Space) = True
isSpace _          = False

isWall (_, Wall) = True
isWall _         = False

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
          $ let g = grid ["#G#####", "#EG.G.#"]
                e = Unit (Elf (AttackPower 0) (HitPoints 0))
                t = adjacentTargets g ((1, 1), e)
            in  case t of
                  [(c1, Unit (Goblin _ _)), (c2, Unit (Goblin _ _))] -> do
                    c1 `Test.shouldBe` ((0, 1) :: (Int, Int))
                    c2 `Test.shouldBe` ((1, 2) :: (Int, Int))
                  _ -> error
                    "we didn't get the number of results that we expected"

        Test.it "should give us any adjacent cells in the right order"
          $ let g = grid ["#G#####", "#EG.G.#"]
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
          $ let g    = grid ["E.", ".G"]
                from = ((0, 0), elf)
                to   = ((0, 1), Space)
                g'   = move g from to
            in  do
                  M.lookup (0, 1) g' `Test.shouldBe` Just elf
                  M.lookup (0, 0) g' `Test.shouldBe` Just Space

        -- this test fails at the moment
        Test.describe "Whole simulation" $ do

          Test.it "test grid one"
            $ let g = grid
                    [ "#######"
                    , "#.G...#"
                    , "#...EG#"
                    , "#.#.#G#"
                    , "#..G#E#"
                    , "#.....#"
                    , "#######"
                    ]
              in  do
                    res <- playGame 0 g
                    res `Test.shouldBe` (47 :: Int, 590 :: Int)

          Test.it "test grid two"
            $ let g = grid
                    [ "#######"
                    , "#G..#E#"
                    , "#E#E.E#"
                    , "#G.##.#"
                    , "#...#E#"
                    , "#...E.#"
                    , "#######"
                    ]
              in  do
                    res <- playGame 0 g
                    res `Test.shouldBe` (37 :: Int, 982 :: Int)

          Test.it "test grid three"
            $ let g = grid
                    [ "#######"
                    , "#E..EG#"
                    , "#.#G.E#"
                    , "#E.##E#"
                    , "#G..#.#"
                    , "#..E#.#"
                    , "#######"
                    ]
              in  do
                    res <- playGame 0 g
                    res `Test.shouldBe` (46 :: Int, 859 :: Int)

          Test.it "test grid four"
            $ let g = grid
                    [ "#######"
                    , "#E.G#.#"
                    , "#.#G..#"
                    , "#G.#.G#"
                    , "#G..#.#"
                    , "#...E.#"
                    , "#######"
                    ]
              in  do
                    res <- playGame 0 g
                    res `Test.shouldBe` (35 :: Int, 793 :: Int)

          Test.it "test grid five"
            $ let g = grid
                    [ "#######"
                    , "#.E...#"
                    , "#.#..G#"
                    , "#.###.#"
                    , "#E#G#G#"
                    , "#...#G#"
                    , "#######"
                    ]
              in  do
                    res <- playGame 0 g
                    res `Test.shouldBe` (54 :: Int, 536 :: Int)

          Test.it "test grid six"
            $ let g = grid
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
                    res <- playGame 0 g
                    res `Test.shouldBe` (20 :: Int, 937 :: Int)

        Test.describe "Path finding" $ do
          let g  = grid ["E.", ".G"]
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
              in  reduceHitPointsBy 3 g1 `Test.shouldBe` g2

          Test.it "reduce hit points should work when fatal"
            $ let g1 = ((1, 5), Unit $ Goblin (AttackPower 3) (HitPoints 3))
                  g2 = ((1, 5), Space)
              in  reduceHitPointsBy 3 g1 `Test.shouldBe` g2


        Test.describe "A slightly larger example grid" $ do
          let g = grid ["#######", "#.E...#", "#.....#", "#...G.#", "#######"]
              from    = ((1, 2), elf)
              to      = ((3, 4), goblin)
              targets = [((3, 3), Space), ((3, 5), Space), ((2, 4), Space)]

          Test.it "should select and make the correct first move"
            $ let g' = findMove g targets from
              in  do
                    M.lookup (1, 3) g' `Test.shouldBe` Just elf
                    M.lookup (1, 2) g' `Test.shouldBe` Just Space

          Test.it "should select and make the correct second move"
            $ let g'  = findMove g targets from
                  g'' = findMove g' targets ((1, 3), elf)
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
            $ let (complete, g') = takeATurn from g
              in  do
                    complete `Test.shouldBe` False
                    M.lookup (1, 3) g' `Test.shouldBe` Just elf
                    M.lookup (1, 2) g' `Test.shouldBe` Just Space

        Test.describe "With the full grid" $ do
          let g    = grid fullGrid
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
            $ let (complete, _g') = takeATurn from g
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
