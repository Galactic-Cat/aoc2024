module Solution2 (solve2, Tile (File, Free)) where
  import Control.Monad (join)
  import Debug.Trace (trace)
  
  data Tile = File Int Int | Free Int

  instance (Show Tile) where
    show (File n s) = join $ replicate s (show n)
    show (Free s)   = replicate s '.'

  checkSum :: [Tile] -> Int -> Int
  checkSum []            _ = 0
  checkSum (Free 0  :ts) i = checkSum ts i
  checkSum (Free s  :ts) i = checkSum (Free (s - 1):ts) (i + 1)
  checkSum (File _ 0:ts) i = checkSum ts i
  checkSum (File x s:ts) i = (x * i) + checkSum (File x (s - 1):ts) (i + 1)

  compact :: [Tile] -> [Tile]
  compact []              = []
  compact (t@(Free s):ts) =
    case pop ts s of
      Nothing        -> t  : compact ts
      Just (t', ts') -> t' : compact ts'
  compact (t         :ts) = t : compact ts

  solve2 :: [Tile] -> Int
  solve2 ts = checkSum (qt $ compact ts) 0

  pop :: [Tile] -> Int -> Maybe (Tile, [Tile])
  pop tss s =
    case pop' (reverse tss) of
      Nothing        -> Nothing
      Just (rt, rts) -> Just (rt, reverse rts)
    where
      pop' :: [Tile] -> Maybe (Tile, [Tile])
      pop' []                = Nothing
      pop' (t@(File _ s'):ts)
        | s' <= s = case ts of
          (Free z : ts') -> Just (t, Free (s' + z): ts')
          _              -> Just (t, Free s'      : ts)
      pop' (t@(Free a)   :ts) =
        case pop' ts of
          Nothing                 -> Nothing
          Just (t', Free b : ts') -> Just (t', Free (a + b) : ts')
          Just (t', ts')          -> Just (t', t:ts')
      pop' (t            :ts) =
        case pop' ts of
          Nothing                 -> Nothing
          Just (t', ts')          -> Just (t', t:ts')

  qt :: Show a => a -> a
  qt x = trace (show x) x