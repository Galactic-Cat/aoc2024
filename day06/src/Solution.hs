module Solution (
  solve,
  solve2,
  Coordinate,
  Direction (North, East, South, West),
  Guard (Guard),
  Tile (Unvisited, Visited, Obstructed),
) where
  import Prelude hiding (filter, iterate, traverse)
  import Debug.Trace (trace)

  type Coordinate = (Int, Int)
  data Direction = North | East | South | West
  data Guard = Guard Coordinate Direction

  data Tile = Unvisited | Visited | VisitedTurned | Obstructed
  instance (Show Tile) where
    show Unvisited     = "."
    show Visited       = "X"
    show Obstructed    = "#"
    show VisitedTurned = "+"

  assert :: String -> Maybe a -> a
  assert _ (Just x) = x
  assert s Nothing  = error s

  countVisited :: [[Tile]] -> Int
  countVisited []      = 0
  countVisited (ts:tg) = countRow ts + countVisited tg
    where
      countRow []                  = 0
      countRow (Visited      :ts') = 1 + countRow ts'
      countRow (VisitedTurned:ts') = 1 + countRow ts'
      countRow (_:ts')             = countRow ts'

  findDirection :: Coordinate -> Coordinate -> Maybe Direction
  findDirection (ax, ay) (bx, by)
    | ax == bx && ay == by = Nothing
    | ax == bx             =
      if   ay > by
      then Just North
      else Just South
    | ay == by             =
      if   ax > bx
      then Just West
      else Just East
    | otherwise            = Nothing

  findSquare :: [[Tile]] -> Coordinate -> Bool
  findSquare tg c = trace ("Finding square for " ++ show c) $
    case (sndTurnPoint, fstTurnPoint) of
      (Just sndc, Just fstc) -> trace ("  2c: " ++ show sndc) $ zz "  1-4: " (unobstructed tg (zz "  1c: " fstc) frtc) && zz "  3-4: " (unobstructed tg c frtc) && zz "  4x: " (isUnvisited tg (zz "  4xp: " $ getPos frtc fstDirection))
        where
          frtc = zz "  4c: " $ findFinalSquarePoint c (rotate curDirection) fstc
      _                   -> trace "  Failure" False
    where
      fstDirection = unrotate sndDirection
      fstTurnPoint = 
        case sndTurnPoint of
          Just stp -> findTurn tg (getPos stp fstDirection) fstDirection
          Nothing  -> Nothing
      sndDirection = rotate $ rotate curDirection
      sndTurnPoint = findTurn tg (getPos c sndDirection) sndDirection
      curDirection = findWalkDirection tg c

  findFinalSquarePoint :: Coordinate -> Direction -> Coordinate -> Coordinate
  findFinalSquarePoint (ax, _ ) North (_ , by) = (ax, by)
  findFinalSquarePoint (_ , ay) East  (bx, _ ) = (bx, ay)
  findFinalSquarePoint (ax, _ ) South (_ , by) = (ax, by)
  findFinalSquarePoint (_ , ay) West  (bx, _ ) = (bx, ay)

  findTurn :: [[Tile]] -> Coordinate -> Direction -> Maybe Coordinate
  findTurn tg c d =
    case getTile tg c of
      Just Obstructed    -> Nothing
      Just VisitedTurned -> Just c
      Just _             -> findTurn tg (getPos c d) d
      Nothing            -> Nothing

  findWalkDirection :: [[Tile]] -> Coordinate -> Direction
  findWalkDirection tg c = check North
    where
      check d =
        case getTile tg (getPos c d) of
          Just Obstructed -> d
          _               -> check (rotate d)

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

  isUnvisited :: [[Tile]] -> Coordinate -> Bool
  isUnvisited tg c =
    case getTile tg c of
      Just Unvisited -> True
      _              -> False

  iterate :: [[Tile]] -> Guard -> ([[Tile]], Maybe Guard)
  iterate tg g@(Guard c d) =
    case getTile tg newpos of
      Nothing         -> (visit tg c, Nothing)
      Just Obstructed -> (visit tg c, Just $ rotateGuard g)
      _               -> (visit tg c, Just $ Guard newpos d)
    where
      newpos = getPos c d

  iterateSquares :: [[Tile]] -> Guard -> ([[Tile]], Maybe Guard, Bool)
  iterateSquares tg g@(Guard c d) =
    case getTile tg newpos of
      Nothing         -> (visit tg c, Nothing              , False)
      Just Obstructed -> (vttg      , Just $ rotateGuard g , findSquare vttg c)
      _               -> (visit tg c, Just $ Guard newpos d, False)
    where
      newpos = getPos c d
      vttg = visitTurn tg c

  iterateComplete :: [[Tile]] -> Guard -> [[Tile]]
  iterateComplete tg g =
    let (tg', g') = iterate tg g
    in  case g' of
        Nothing  -> tg'
        Just g'' -> iterateComplete tg' g''

  iterateCompleteSquares :: [[Tile]] -> Guard -> Int
  iterateCompleteSquares tg g =
    let (tg', g', q) = iterateSquares tg g
    in  case g' of
        Nothing  -> fromEnum q
        Just g'' -> fromEnum q + iterateCompleteSquares tg' g''

  rotate :: Direction -> Direction
  rotate North = East
  rotate East  = South
  rotate South = West
  rotate West  = North

  rotateGuard :: Guard -> Guard
  rotateGuard (Guard c d) = Guard c (rotate d)

  solve :: ([[Tile]], Guard) -> Int
  solve (tg, g) = countVisited $ iterateComplete tg g

  solve2 :: ([[Tile]], Guard) -> Int
  solve2 (tg, g) = iterateCompleteSquares tg g

  unobstructed :: [[Tile]] -> Coordinate -> Coordinate -> Bool
  unobstructed tg a@(ax, ay) b@(bx, by)
    | ax == bx && ay == by = True
    | otherwise            =
      case getTile tg a of
        Just Obstructed -> trace ("  Obstruction at " ++ show a) False
        Just _          -> unobstructed tg (getPos a d) b
        Nothing         -> False
      where
        d = assert "unobstucted couldn't find direction between point a and b" $ findDirection a b

  unrotate :: Direction -> Direction
  unrotate North = West
  unrotate East  = North
  unrotate South = East
  unrotate West  = South

  visit :: [[Tile]] -> Coordinate -> [[Tile]]
  visit []          _      = error "Visit coordinate invalid"
  visit ((VisitedTurned:ts):tg) (0, 0) = (VisitedTurned : ts) : tg
  visit ((_            :ts):tg) (0, 0) = (Visited       : ts) : tg
  visit ((t            :ts):tg) (x, 0) =
    let tg' = visit (ts:tg) (x - 1, 0)
    in  ((t:head tg'):tg)
  visit (ts                :tg) (x, y) = ts : visit tg (x, y - 1)

  visitTurn :: [[Tile]] -> Coordinate -> [[Tile]]
  visitTurn []          _      = error "Visit coordinate invalid"
  visitTurn ((_:ts):tg) (0, 0) = (VisitedTurned : ts) : tg
  visitTurn ((t:ts):tg) (x, 0) =
    let tg' = visitTurn (ts:tg) (x - 1, 0)
    in  ((t:head tg'):tg)
  visitTurn (ts:tg)     (x, y) = ts: visitTurn tg (x, y - 1)

  zz :: Show a => String -> a -> a
  zz s x = trace (s ++ show x) x