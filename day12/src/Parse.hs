module Parse (parse) where
  import Solution (Grid, Plot (Plot), Row)

  parse :: String -> IO Grid
  parse path =
    do
      contents <- readFile path
      return . lines $ contents