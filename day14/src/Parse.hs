module Parse (parse) where
  import Text.Regex.TDFA ((=~), getAllTextMatches)

  parse :: String -> IO [((Int, Int), (Int, Int))]
  parse path =
    do
      contents <- readFile path
      return . map parseLine . lines $ contents

  parseLine :: String -> ((Int, Int), (Int, Int))
  parseLine s =
    case getAllTextMatches (s =~ "\\-?[0-9]+") of
      [px, py, vx, vy] -> ((read px, read py), (read vx, read vy))
      _                -> error "Could not parse line"