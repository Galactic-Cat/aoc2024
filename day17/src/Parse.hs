module Parse (parse) where
  import Text.Regex.TDFA ((=~), getAllTextMatches)
  import Solution (Tape)

  parse :: String -> IO (Int, Int, Int, Tape)
  parse path =
    do
      contents <- readFile path
      return . parseInput $ getAllTextMatches (contents =~ "[0-9]+")


  parseInput :: [String] -> (Int, Int, Int, Tape)
  parseInput (a:b:c:t) = (read a, read b, read c, map read t)
  parseInput s         = error $ "Failed to parse: " ++ show s