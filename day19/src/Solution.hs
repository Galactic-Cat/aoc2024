module Solution (solve) where
  import Debug.Trace (trace)

  import qualified Tree as T
  
  countPossible :: T.PatternTree -> [String] -> Int
  countPossible _ []     = 0
  countPossible t (l:ls) =
    if   T.matchFull l t
    then 1 + countPossible t ls
    else countPossible t ls

  solve :: (T.PatternTree, [String]) -> Int
  solve (t, ls) = countPossible t ls

  zz :: Show a => a -> a
  zz x = trace (show x) x