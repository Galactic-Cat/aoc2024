module Tree (Pattern, PatternTree, add, empty) where
  import Data.Maybe (fromMaybe)

  data Pattern = White PatternTree | Blue PatternTree | Black PatternTree | Red PatternTree | Green PatternTree
  type PatternTree = (Maybe Pattern, Maybe Pattern, Maybe Pattern, Maybe Pattern, Maybe Pattern)

  add :: String -> PatternTree -> PatternTree
  add []       t = t
  add ('w':cs) t = (Just (inSubTree (getSafe t 0) (add cs)), getU t, getB t, getR t, getG t)
  add ('u':cs) t = (getW t, Just (inSubTree (getSafe t 1) (add cs)), getB t, getR t, getG t)
  add ('b':cs) t = (getW t, getU t, Just (inSubTree (getSafe t 2) (add cs)), getR t, getG t)
  add ('r':cs) t = (getW t, getU t, getB t, Just (inSubTree (getSafe t 3) (add cs)), getG t)
  add ('g':cs) t = (getW t, getU t, getB t, getR t, Just (inSubTree (getSafe t 4) (add cs)))
  add (_  :_ ) _ = error "Pattern character not recognized in add"

  empty :: PatternTree
  empty = (Nothing, Nothing, Nothing, Nothing, Nothing)

  getB :: (a, b, c, d, e) -> c
  getB (_, _, b, _, _) = b

  getG :: (a, b, c, d, e) -> e
  getG (_, _, _, _, g) = g
  
  getR :: (a, b, c, d, e) -> d
  getR (_, _, _, r, _) = r

  getSafe :: (Maybe Pattern, Maybe Pattern, Maybe Pattern, Maybe Pattern, Maybe Pattern) -> Int -> Pattern
  getSafe (w, _, _, _, _) 0 = fromMaybe (White empty) w
  getSafe (_, u, _, _, _) 1 = fromMaybe (White empty) u
  getSafe (_, _, b, _, _) 2 = fromMaybe (White empty) b
  getSafe (_, _, _, r, _) 3 = fromMaybe (White empty) r
  getSafe (_, _, _, _, g) 4 = fromMaybe (White empty) g
  getSafe _               _ = error "getSafe int out of range"

  getU :: (a, b, c, d, e) -> b
  getU (_, u, _, _, _) = u
  
  getW :: (a, b, c, d, e) -> a
  getW (w, _, _, _, _) = w
  
  inSubTree :: Pattern -> (PatternTree -> PatternTree) -> Pattern
  inSubTree (White t) f = White $ f t
  inSubTree (Blue  t) f = Blue  $ f t
  inSubTree (Black t) f = Black $ f t
  inSubTree (Red   t) f = Red   $ f t
  inSubTree (Green t) f = Green $ f t