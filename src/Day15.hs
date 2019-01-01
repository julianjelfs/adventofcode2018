{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
module Day15 where

import qualified Data.List                     as L
import qualified Data.Map.Strict               as M
import qualified Data.Set                      as S

newtype AttackPower = AttackPower Int deriving (Show, Eq, Ord)

newtype HitPoints = HitPoints Int deriving (Show, Eq, Ord)

data Combatant
    = Goblin AttackPower HitPoints
    | Elf AttackPower HitPoints
    deriving (Show, Eq, Ord)

data Cell
    = Unit Combatant
    | Space
    deriving (Show, Eq, Ord)

type Coord = (Int, Int)

type Pair = (Coord, Cell)

type Path = [Pair]

type Grid = M.Map Coord Cell

partOne :: IO (Int, Int, Int, Int)
partOne = grid 3 . lines <$> readFile "data/day15.txt" >>= playGame False 0

-- yeah I just ran this repeatedly until I got the answer
partTwo :: Int -> Bool -> IO (Int, Int, Int, Int)
partTwo elfAttackPower elfDeathFatal =
  grid elfAttackPower . lines <$> readFile "data/day15.txt" >>= playGame
    elfDeathFatal
    0

grid :: Int -> [String] -> Grid
grid elfAttackPower rows = L.foldl'
  (\m (y, row) -> L.foldl'
    (\m (x, c) -> case parseCell elfAttackPower c of
      Nothing   -> m
      Just cell -> M.insert (y, x) cell m
    )
    m
    (zip [0 ..] row)
  )
  M.empty
  (zip [0 ..] rows)

playGame :: Bool -> Int -> Grid -> IO (Int, Int, Int, Int)
playGame elfDeathFatal rounds g = runARound elfDeathFatal g >>= \case
  (False, g') -> playGame elfDeathFatal (rounds + 1) g'
  (True, g') ->
    pure (rounds, sumHitPoints g', numberOfElves g', numberOfGoblins g')

numberOfElves :: Grid -> Int
numberOfElves = length . elves

numberOfGoblins :: Grid -> Int
numberOfGoblins = length . goblins

sumHitPoints :: Grid -> Int
sumHitPoints = M.foldr
  (\cell total -> case cell of
    Unit (Goblin _ (HitPoints h)) -> total + h
    Unit (Elf    _ (HitPoints h)) -> total + h
    _                             -> total
  )
  0

runARound :: Bool -> Grid -> IO (Bool, Grid)
runARound elfDeathFatal g = pure $ L.foldl'
  (\(complete, g') u ->
    if complete then (complete, g') else takeATurn elfDeathFatal u g'
  )
  (False, g)
  (units g)

takeATurn :: Bool -> Pair -> Grid -> (Bool, Grid)
takeATurn elfDeathFatal (uc, _) g@(gridToList -> listGrid) =
  case M.lookup uc g of
    Nothing    -> (False, g)
    Just Space -> (False, g)
    Just u ->
      let unit = (uc, u)
      in  case adjacentTargets g unit of
            [] -> case targets unit listGrid of
              [] -> (True, g) -- This is game over
              ts -> case allAvailableAdjacentCells g ts of
                [] -> (False, g) -- there are targets but none of them have adjacent free cells
                targetCells ->
                  (False, findMove elfDeathFatal g targetCells unit)
            adj -> attack elfDeathFatal unit adj g

attack :: Bool -> Pair -> [Pair] -> Grid -> (Bool, Grid)
attack elfDeathFatal unit (weakest -> w@(wc, _)) g =
  let ((_, attacked), elfDeath) = reduceHitPointsBy (extractAttackPower unit) w
  in  (elfDeathFatal && elfDeath, M.update (\_ -> Just attacked) wc g)

reduceHitPointsBy :: Int -> Pair -> (Pair, Bool)
reduceHitPointsBy n (c, Unit (Goblin ap (HitPoints x)))
  | x - n <= 0 = ((c, Space), False)
  | otherwise  = ((c, Unit (Goblin ap (HitPoints (x - n)))), False)
reduceHitPointsBy n (c, Unit (Elf ap (HitPoints x)))
  | x - n <= 0 = ((c, Space), True)
  | otherwise  = ((c, Unit (Elf ap (HitPoints (x - n)))), False)
reduceHitPointsBy _ p = (p, False)

extractAttackPower :: Pair -> Int
extractAttackPower (_, Unit (Goblin (AttackPower n) _)) = n
extractAttackPower (_, Unit (Elf (AttackPower n) _)) = n
extractAttackPower _ = 0

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


findMove :: Bool -> Grid -> [Pair] -> Pair -> Grid
findMove elfDeathFatal g possibleTargets unit =
  case findBestTarget g unit possibleTargets of
    Nothing -> g
    Just t  -> case shortestPath g unit (t, Space) of
      Nothing        -> g
      Just (_, _, p) -> case p of
        (to : _) -> move elfDeathFatal g unit to
        _        -> g

move :: Bool -> Grid -> Pair -> Pair -> Grid
move elfDeathFatal g (k, v) (k', _) =
  let newUnit = (k', v)
      updated = M.update (\_ -> Just Space) k (M.update (\_ -> Just v) k' g)
  in  case adjacentTargets updated newUnit of
        []  -> updated
        adj -> snd $ attack elfDeathFatal newUnit adj updated

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
          else if minLength paths < length h
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

  minLength :: [Path] -> Int
  minLength []    = maxBound
  minLength paths = minimum $ length <$> paths


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

goblins :: Grid -> [Pair]
goblins = L.filter isGoblin . gridToList

elves :: Grid -> [Pair]
elves = L.filter isElf . gridToList

targets :: Pair -> [Pair] -> [Pair]
targets unit = L.filter (\u -> u /= unit && u `isTargetOf` unit)


isTargetOf :: Pair -> Pair -> Bool
isTargetOf (_, Unit (Goblin _ _)) (_, Unit (Elf _ _)   ) = True
isTargetOf (_, Unit (Elf _ _)   ) (_, Unit (Goblin _ _)) = True
isTargetOf _                      _                      = False


adjacentTargets :: Grid -> Pair -> [Pair]
adjacentTargets g c = L.filter (isTargetOf c) $ adjacentCells g c


isGoblin, isElf, isUnit, isSpace :: Pair -> Bool
isUnit (_, Unit _) = True
isUnit _           = False

isGoblin (_, Unit (Goblin _ _)) = True
isGoblin _                      = False

isElf (_, Unit (Elf _ _)) = True
isElf _                   = False

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


parseCell :: Int -> Char -> Maybe Cell
parseCell elfAttackPower = \case
  '#' -> Nothing
  'G' -> Just . Unit $ Goblin (AttackPower 3) (HitPoints 200)
  'E' -> Just . Unit $ Elf (AttackPower elfAttackPower) (HitPoints 200)
  '.' -> Just Space
  _   -> error "parse error"
