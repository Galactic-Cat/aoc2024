module Parse (parse) where
  import Debug.Trace (trace)
  import Text.Regex.TDFA ((=~), getAllTextMatches)

  import qualified Tree as T

  parse :: String -> IO (T.PatternTree, [String])
  parse path =
    do
      contents <- readFile path
      return . (\ls -> (zz $ parsePatterns (getAllTextMatches (head ls =~ "[wubgr]+")), filter (not . null) (tail ls))) . lines $ contents

  parsePatterns :: [String] -> T.PatternTree
  parsePatterns = foldr T.add T.empty

  zz :: Show a => a -> a
  zz x = trace (show x) x