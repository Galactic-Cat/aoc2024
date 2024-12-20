module Parse (parse) where
  import qualified Data.Set as S
  import Solution (Coordinate, Space, Walls)

  assert4s2 :: (a, b, Maybe c, Maybe d) -> (a, b, c, d)
  assert4s2 (_, _, Nothing, _      ) = error "Could not assert third value"
  assert4s2 (_, _, _      , Nothing) = error "Could not assert fourth value"
  assert4s2 (a, b, Just c , Just d ) = (a, b, c ,d)

  best :: Maybe a -> Maybe a -> Maybe a
  best Nothing  Nothing  = Nothing
  best (Just a) _        = Just a
  best Nothing  (Just b) = Just b 

  parse :: String -> IO (Space, Walls, Coordinate, Coordinate)
  parse path =
    do
      contents <- readFile path
      return . assert4s2 . parseInput 0 . lines $ contents

  parseInput :: Int -> [String] -> (Space, Walls, Maybe Coordinate, Maybe Coordinate)
  parseInput _ []     = (S.empty, S.empty, Nothing, Nothing)
  parseInput y (l:ls) =
    let (s, w, i, o) = parseInput (y + 1) ls in
    let (s', w', i', o') = parseLine 0 l in
      (S.union s s', S.union w w', best i i', best o o')
    where
      parseLine _ []     = (S.empty, S.empty, Nothing, Nothing)
      parseLine x (c:cs) = 
        let (s, w, i, o) = parseLine (x + 1) cs in
          case c of
            '.' -> (S.insert (x, y) s, w                , i          , o          )
            '#' -> (s                , S.insert (x, y) w, i          , o          )
            'S' -> (S.insert (x, y) s, w                , Just (x, y), o          )
            'E' -> (S.insert (x, y) s, w                , i          , Just (x, y))
            _   -> error $ "Character " ++ c : " not recognized"
