module Parse (parse) where
  import Solution (
    Coordinate,
    Direction (North, East, South, West),
    Guard (Guard),
    Tile (Unvisited, Visited, Obstructed))

  best :: Maybe a -> Maybe a -> Maybe a
  best (Just x) _ = Just x
  best _ (Just y) = Just y
  best _        _ = Nothing

  parse :: String -> IO ([[Tile]], Guard)
  parse path =
    do
      contents <- readFile path
      return $ parseWorld (lines contents)

  parseLine :: String -> Coordinate -> ([Tile], Maybe Guard)
  parseLine []     _        = ([], Nothing)
  parseLine (x:xs) (cx, cy) =
    let (ws, gs) = parseLine xs (cx + 1, cy)
    in  case x of
        '.' -> (Unvisited : ws, gs)
        '#' -> (Obstructed : ws, gs)
        '^' -> (Visited : ws, Just $ Guard (cx, cy) North)
        '>' -> (Visited : ws, Just $ Guard (cx, cy) East)
        'v' -> (Visited : ws, Just $ Guard (cx, cy) South)
        '<' -> (Visited : ws, Just $ Guard (cx, cy) West)
        _   -> error $ "Unrecogonized character '" ++ [x] ++ "'"

  parseWorld :: [String] -> ([[Tile]], Guard)
  parseWorld s = 
    let (ls, g) = lined (length s - 1)
    in  (ls, assert g)
    where
      assert (Just x) = x
      assert Nothing  = error "Guard not found"

      lined 0 = let (l, g) = parseLine (head s) (0, 0) in ([l], g)
      lined y =
        let (ls, gs) = lined (y - 1)
        in  let (l,  g) = parseLine (s !! y) (0, y)
            in  (l : ls, best g gs)