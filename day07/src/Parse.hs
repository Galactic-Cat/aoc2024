module Parse (parse) where
  import Text.Regex.TDFA ((=~), getAllTextMatches)

  numberRegex :: String
  numberRegex = "[0-9]+"

  parse :: String -> IO [[Int]]
  parse path =
    do
      contents <- readFile path
      return $ map (map read . (\l -> getAllTextMatches (l =~ numberRegex))) (lines contents)