module Parse (parse) where
  import Text.Regex.TDFA ((=~), getAllTextMatches)

  parse :: String -> IO ([String], [String])
  parse path =
    do
      contents <- readFile path
      return . parseInput . (\ls -> (head ls, filter (not . null) (tail ls))) . lines $ contents

  parseInput :: (String, [String]) -> ([String], [String])
  parseInput (s, xs) = (getAllTextMatches (s =~ "[wubrg]+"), xs)