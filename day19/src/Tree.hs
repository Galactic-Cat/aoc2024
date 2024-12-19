module Tree (Pattern, PatternTree, add, matchFull, empty) where
  import Data.Maybe (fromMaybe)

  data Pattern = Pattern Char PatternTree
  type PatternTree = (Maybe Pattern, Maybe Pattern, Maybe Pattern, Maybe Pattern, Maybe Pattern, Bool)

  add :: String -> PatternTree -> PatternTree
  add []       t = (getW t, getU t, getB t, getR t, getG t, True)
  add ('w':cs) t = (Just (inSubTree (getSafe t 0) (add cs)), getU t, getB t, getR t, getG t, getEnd t)
  add ('u':cs) t = (getW t, Just (inSubTree (getSafe t 1) (add cs)), getB t, getR t, getG t, getEnd t)
  add ('b':cs) t = (getW t, getU t, Just (inSubTree (getSafe t 2) (add cs)), getR t, getG t, getEnd t)
  add ('r':cs) t = (getW t, getU t, getB t, Just (inSubTree (getSafe t 3) (add cs)), getG t, getEnd t)
  add ('g':cs) t = (getW t, getU t, getB t, getR t, Just (inSubTree (getSafe t 4) (add cs)), getEnd t)
  add (_  :_ ) _ = error "Pattern character not recognized in add"

  charToInt :: Char -> Int
  charToInt 'w' = 0
  charToInt 'u' = 1
  charToInt 'b' = 2
  charToInt 'r' = 3
  charToInt 'g' = 4
  charToInt _   = error "charToInt char not recognized"

  empty :: PatternTree
  empty = (Nothing, Nothing, Nothing, Nothing, Nothing, False)

  get :: PatternTree -> Int -> Maybe Pattern
  get (w, _, _, _, _, _) 0 = w
  get (_, u, _, _, _, _) 1 = u
  get (_, _, b, _, _, _) 2 = b
  get (_, _, _, r, _, _) 3 = r
  get (_, _, _, _, g, _) 4 = g
  get _                  _ = error "get int out of range"

  getB :: (a, b, c, d, e, f) -> c
  getB (_, _, b, _, _, _) = b

  getEnd :: (a, b, c, d, e, f) -> f
  getEnd (_, _, _, _, _, e) = e

  getG :: (a, b, c, d, e, f) -> e
  getG (_, _, _, _, g, _) = g

  getR :: (a, b, c, d, e, f) -> d
  getR (_, _, _, r, _, _) = r

  getSafe :: PatternTree -> Int -> Pattern
  getSafe (w, _, _, _, _, _) 0 = fromMaybe (Pattern 'w' empty) w
  getSafe (_, u, _, _, _, _) 1 = fromMaybe (Pattern 'u' empty) u
  getSafe (_, _, b, _, _, _) 2 = fromMaybe (Pattern 'b' empty) b
  getSafe (_, _, _, r, _, _) 3 = fromMaybe (Pattern 'r' empty) r
  getSafe (_, _, _, _, g, _) 4 = fromMaybe (Pattern 'g' empty) g
  getSafe _                  _ = error "getSafe int out of range"

  getU :: (a, b, c, d, e, f) -> b
  getU (_, u, _, _, _, _) = u

  getW :: (a, b, c, d, e, f) -> a
  getW (w, _, _, _, _, _) = w

  inSubTree :: Pattern -> (PatternTree -> PatternTree) -> Pattern
  inSubTree (Pattern c t) f = Pattern c $ f t

  isEmpty :: PatternTree -> Bool
  isEmpty (Nothing, Nothing, Nothing, Nothing, Nothing, _) = True
  isEmpty _                                                = False

  matchFull :: String -> PatternTree -> Bool
  matchFull []     t = getEnd t
  matchFull (c:cs) t =
    case get t (charToInt c) of
      Just (Pattern _ pt) -> matchFull cs pt
      Nothing             -> False

  match :: String -> PatternTree -> [(String, String)]
  match = match' []
    where
      match' :: String -> String -> PatternTree -> [(String, String)]
      match' a []     t = [(a, []) | getEnd t]
      match' a (c:cs) t = _

        where
          ms =
            if   getEnd t
            then (a ++ [c], cs)
            else
        
