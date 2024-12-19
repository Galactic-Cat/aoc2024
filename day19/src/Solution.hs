module Solution (solve) where  
  import Data.List (intercalate)
  import Text.Regex.TDFA ((=~))
  
  countPossible :: String -> [String] -> Int
  countPossible _ []     = 0
  countPossible p (l:ls) =
    if   l =~ p
    then 1 + countPossible p ls
    else countPossible p ls

  combineRegex :: [String] -> String
  combineRegex xs = "\\`(" ++ intercalate "|" xs ++ ")+\\'"

  solve :: ([String], [String]) -> Int
  solve (ps, ls) = countPossible (combineRegex ps) ls