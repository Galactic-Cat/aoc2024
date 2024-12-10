module Solution2 (solve2, Tile (File, Free)) where
  data Tile = File Int Int | Free Int

  checkSum :: [Tile] -> Int -> Int
  checkSum []            _ = 0
  checkSum (Free 0  :ts) i = checkSum ts i
  checkSum (Free s  :ts) i = checkSum (Free (s - 1):ts) (i + 1)
  checkSum (File _ 0:ts) i = checkSum ts i
  checkSum (File x s:ts) i = (x * i) + checkSum (File x (s - 1):ts) (i + 1)

  compact :: [Tile] -> [Tile]
  compact [] = []
  compact (t@(File _ s):ts) =
    case fit t (reverse ts) of
      Just ts' -> compact (Free s : reverse (joinFree ts'))
      Nothing  -> t : compact ts
  compact (t           :ts) = t : compact ts

  fit :: Tile -> [Tile] -> Maybe [Tile]
  fit _            []                = Nothing
  fit f            (t@(File _ _):ts) =
    case fit f ts of
      Just ts' -> Just $ t : ts'
      Nothing  -> Nothing
  fit f@(File _ s) (t@(Free z)  :ts)
    | s == z    = Just $ f : ts
    | s < z     = Just $ f : Free (z - s) : ts
    | otherwise =
      case fit f ts of
        Just ts' -> Just $ t : ts'
        Nothing  -> Nothing
  fit _            _                = error "Impossible state for fit"

  joinFree :: [Tile] -> [Tile]
  joinFree []                   = []
  joinFree (Free as:Free bs:ts) = joinFree (Free (as + bs):ts)
  joinFree (t      :ts)         = t : joinFree ts

  solve2 :: [Tile] -> Int
  solve2 ts = checkSum (reverse $ compact (reverse ts)) 0
