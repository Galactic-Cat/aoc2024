module Parse (parse) where
  import Text.Regex.TDFA ((=~), getAllTextMatches)

  parse :: String -> IO [(String, String)]
  parse path =
    do
      contents <- readFile path
      return . map parseLine . lines $ contents

  parseLine :: String -> (String, String)
  parseLine s =
    case getAllTextMatches (s =~ "[A-Za-z]+") of
      (a : b : _) -> (a, b)
      _           -> error $ "Failed to parse line: " ++ s