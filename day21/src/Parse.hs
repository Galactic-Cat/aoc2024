module Parse (parse) where
  parse :: String -> IO [String]
  parse path =
    do
      contents <- readFile path
      return . lines $ contents