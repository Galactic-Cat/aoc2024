module Solution (solve, Tile (File, Free)) where  
  data Tile = File Int | Free

  instance (Show Tile) where
    show (File n) = show n
    show Free     = "."

  checkSum :: [Tile] -> Int -> Int
  checkSum []          _ = 0
  checkSum (Free  :ts) i = checkSum ts (i + 1)
  checkSum (File n:ts) i = n * i + checkSum ts (i + 1)

  compact :: [Tile] -> [Tile]
  compact []        = []
  compact (Free:ts) =
    case pop ts of
      Nothing       -> Free:ts
      Just (t, ts') -> t : compact ts'
  compact (t   :ts) = t : compact ts

  solve :: [Tile] -> Int
  solve ts = checkSum (compact ts) 0

  pop :: [Tile] -> Maybe (Tile, [Tile])
  pop tss =
    case pop' (reverse tss) of
      Nothing        -> Nothing
      Just (rt, rts) -> Just (rt, reverse rts)
    where
      pop' []              = Nothing
      pop' (t@(File _):ts) = Just (t, Free : ts)
      pop' (t         :ts) =
        case pop' ts of
          Nothing        -> Nothing
          Just (t', ts') -> Just (t', t:ts')