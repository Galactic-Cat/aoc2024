module Parse (parse) where
  import Text.Regex.TDFA ((=~), getAllTextMatches)

  import qualified Tree as T

  parse :: String -> IO ([String], [String])
  parse path =
    do
      contents <- readFile path
      return . parseInput . (\ls -> (head ls, filter (not . null) (tail ls))) . lines $ contents

  -- parse2 :: String -> (T.PatternTree, [String])
  -- parse2 path =
  --   do
  --     contents <- readFile path
  --     return . (\ls -> (parsePatterns (getAllTextMatches (head ls =~ "[wubrg]+")), filter (not . null) (tail ls))) . lines $ contents

  parseInput :: (String, [String]) -> ([String], [String])
  parseInput (s, xs) = (getAllTextMatches (s =~ "[wubrg]+"), xs)

  -- parsePatterns :: [String] -> T.PatternTree
  -- parsePatterns = foldr addPattern T.empty