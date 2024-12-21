module Parse (parse) where
  import qualified Data.Set as S
  import Solution (Coordinate, Space)

  assert2s1 :: (a, Maybe b) -> (a, b)
  assert2s1 (_, Nothing) = error "Could not assert third value"
  assert2s1 (a, Just b ) = (a, b)

  best :: Maybe a -> Maybe a -> Maybe a
  best Nothing  Nothing  = Nothing
  best (Just a) _        = Just a
  best Nothing  (Just b) = Just b 

  parse :: String -> IO (Space, Coordinate)
  parse path =
    do
      contents <- readFile path
      return . assert2s1 . parseInput 0 . lines $ contents

  parseInput :: Int -> [String] -> (Space, Maybe Coordinate)
  parseInput _ []     = (S.empty, Nothing)
  parseInput y (l:ls) =
    let (s , i ) = parseInput (y + 1) ls in
    let (s', i') = parseLine 0 l in
      (S.union s s', best i i')
    where
      parseLine _ []     = (S.empty, Nothing)
      parseLine x (c:cs) = 
        let (s, i) = parseLine (x + 1) cs in
          case c of
            '.' -> (S.insert (x, y) s, i          )
            '#' -> (s                , i          )
            'S' -> (S.insert (x, y) s, Just (x, y))
            'E' -> (S.insert (x, y) s, i          )
            _   -> error $ "Character " ++ c : " not recognized"
