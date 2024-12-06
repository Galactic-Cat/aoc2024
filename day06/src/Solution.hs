module Solution (
  solve,
  Coordinate,
  Direction (North, East, South, West),
  Guard (Guard),
  Tile (Unvisited, Visited, Obstructed),
) where

  type Coordinate = (Int, Int)
  data Direction = North | East | South | West
  data Guard = Guard Coordinate Direction
  data Tile = Unvisited | Visited | Obstructed

  getPos :: Coordinate -> Direction -> Coordinate
  getPos (x, y) North = (x - 1, y)
  getPos (x, y) East  = (x,     y + 1)
  getPos (x, y) South = (x + 1, y)
  getPos (x, y) West  = (x,     y - 1)

  getTile :: [[Tile]] -> Coordinate -> Maybe Tile
  getTile tg (x, y) =
    if   x >= 0 && y >= 0 && x < length (head tg) && y < length tg
    then Just $ (tg !! y) !! x
    else Nothing

  iterate :: [[Tile]] -> Guard -> ([[Tile]], Maybe Guard)

  solve :: ([[Tile]], Guard) -> IO ()
  solve = print "No solution"
