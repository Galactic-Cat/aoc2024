module Parse (parse) where
  import Solution (Grid, Row, Tile (Tile))
  
  parse :: String -> IO Grid
  parse path =
    do
      contents <- readFile path
      return $ map parseLine (lines contents)

  parseLine :: String -> Row
  parseLine = map (Tile . read . (: []))