module Parse (parse) where
  import qualified Data.Map as Map
  import Solution (Coordinate, Grid (Grid), Tile (Floor, Goal))

  assert3s1 :: (a, Maybe b, Maybe c) -> (a, b, c)
  assert3s1 (_, Nothing, _      ) = error "[assert3s1] second value in tuple is Nothing"
  assert3s1 (_, _      , Nothing) = error "[assert3s1] third value in tuple is Nothing"
  assert3s1 (a, Just b , Just c ) = (a, b, c)

  best :: Maybe a -> Maybe a -> Maybe a
  best Nothing b       = b
  best a       _       = a

  parse :: String -> IO (Grid, Coordinate, Coordinate)
  parse path =
    do
      contents <- readFile path
      return . assert3s1 . parseGrid 0 . lines $ contents

  parseGrid :: Int -> [String] -> (Grid, Maybe Coordinate, Maybe Coordinate)
  parseGrid y []     = (Grid Map.empty 0 y, Nothing, Nothing)
  parseGrid y (r:rs) =
    let (Grid m h _, s, e) = parseGrid (y + 1) rs in
    let (m', s', e') = parseLine 0 r in
      (Grid (Map.union m m') h (length r), best s s', best e e')
    where
      parseLine _ []       = (Map.empty, Nothing, Nothing)
      parseLine x ('S':ts) =
        let (m', _ , e') = parseLine (x + 1) ts in
          (Map.insert (x, y) Floor m', Just (x, y), e'         )
      parseLine x ('E':ts) =
        let (m', s', _ ) = parseLine (x + 1) ts in
          (Map.insert (x, y) Goal  m', s'         , Just (x, y))
      parseLine x ('.':ts) =
        let (m', s', e') = parseLine (x + 1) ts in
          (Map.insert (x, y) Floor m', s'         , e'         )
      parseLine x (_  :ts) = parseLine (x + 1) ts