module Parse (parse) where
  import Data.List (nub)
  import Text.Regex.TDFA ((=~), getAllTextMatches)
  
  import Solution (Grid, Row, Tile (Antenna, Empty))
  
  getLabels :: String -> [Char]
  getLabels x = map head (nub (getAllTextMatches (x =~ "[0-9A-Za-z]")))

  parse :: String -> IO (Grid, [Char])
  parse path =
    do
      contents <- readFile path
      return (map parseLine (lines contents), getLabels contents)

  parseLine :: String -> Row
  parseLine []       = []
  parseLine ('.':cs) = Empty     : parseLine cs
  parseLine (c  :cs) = Antenna c : parseLine cs