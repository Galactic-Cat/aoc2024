module Solution (
  solve,
  Coordinate,
  Direction (North, East, South, West),
  Guard (Guard),
  Tile (Unvisited, Visited, Obstructed),
) where
  import Debug.Trace (trace)
  import Prelude hiding (iterate)
  import Control.Monad (join)
  
  type Coordinate = (Int, Int)
  data Direction = North | East | South | West
  data Guard = Guard Coordinate Direction

  data Tile = Unvisited | Visited | VisitedTurned | Obstructed
  instance (Show Tile) where
    show Unvisited     = "."
    show Visited       = "X"
    show Obstructed    = "#"
    show VisitedTurned = "+"

  showGrid :: [[Tile]] -> String
  showGrid tg = unlines (map (join . map show) tg)

  countVisited :: [[Tile]] -> Int
  countVisited []      = 0
  countVisited (ts:tg) = countRow ts + countVisited tg
    where
      countRow []            = 0
      countRow (Visited:ts') = 1 + countRow ts'
      countRow (_:ts')       = countRow ts'

  getPos :: Coordinate -> Direction -> Coordinate
  getPos (x, y) North = (x,     y - 1)
  getPos (x, y) East  = (x + 1, y)
  getPos (x, y) South = (x,     y + 1)
  getPos (x, y) West  = (x - 1, y)

  getTile :: [[Tile]] -> Coordinate -> Maybe Tile
  getTile tg (x, y) =
    if   x >= 0 && y >= 0 && x < length (head tg) && y < length tg
    then Just $ (tg !! y) !! x
    else Nothing

  iterate :: [[Tile]] -> Guard -> ([[Tile]], Maybe Guard)
  iterate tg g@(Guard c d) =
    case getTile tg newpos of
      Nothing         -> (visit tg c, Nothing)
      Just Obstructed -> (visit tg c, Just $ rotateGuard g)
      _               -> (visit tg c, Just $ Guard newpos d)
    where
      newpos = getPos c d

  iterateComplete :: [[Tile]] -> Guard -> [[Tile]]
  iterateComplete tg g =
    let (tg', g') = iterate tg g
    in  case g' of
        Nothing  -> tg'
        Just g'' -> iterateComplete tg' g''

  rotateGuard :: Guard -> Guard
  rotateGuard (Guard c North) = Guard c East
  rotateGuard (Guard c East)  = Guard c South
  rotateGuard (Guard c South) = Guard c West
  rotateGuard (Guard c West)  = Guard c North

  solve :: ([[Tile]], Guard) -> Int
  solve (tg, g) = countVisited $ iterateComplete tg g

  visit :: [[Tile]] -> Coordinate -> [[Tile]]
  visit []          _      = error "Visit coordinate invalid"
  visit ((_:ts):tg) (0, 0) = (Visited : ts) : tg
  visit ((t:ts):tg) (x, 0) =
    let tg' = visit (ts:tg) (x - 1, 0)
    in  ((t:head tg'):tg)
  visit (ts:tg)     (x, y) = ts: visit tg (x, y - 1)