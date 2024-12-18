module Parse (parse) where
  import Text.Regex.TDFA (getAllTextMatches, (=~))
  import Solution (Coordinate)

  parse :: String -> IO [Coordinate]
  parse path =
    do
      contents <- readFile path
      return . parseLines . lines $ contents

  parseLines :: [String] -> [Coordinate]
  parseLines = map (\l -> parseCoordinate (getAllTextMatches (l =~ "[0-9]+")))
    where
      parseCoordinate [x, y] = (read x, read y)
      parseCoordinate _      = error "No values to parse coordinate"
