module Solution (solve, solve2, Rule (Rule)) where
  import Prelude hiding (traverse)

  data Rule = Rule Int Int
    deriving Show

  inplace :: Int -> [Rule] -> [Int] -> [Int] -> Bool
  inplace x rs ps ns = validateRules (ps ++ [x] ++ ns) rs

  middle :: [a] -> a
  middle xs = xs !! (length xs `div` 2)

  place :: Int -> [Rule] -> [Int] -> [Int]
  place x rs = place' []
    where
      place' ps ns
        | inplace x rs ps ns = x : ns
        | otherwise          = case ns of
                                 []      -> error "Failed to place"
                                 (n:nss) -> n : place' (ps ++ [n]) nss

  placeAll :: [Int] -> [Int] -> [Rule] -> [Int]
  placeAll xs a rs = foldl (\a' x -> place x rs a') a xs

  solve :: ([Rule], [[Int]]) -> Int
  solve (_ , [])   = 0
  solve (rs, x:xs)
    | validateRules x rs = middle x + solve (rs, xs)
    | otherwise          = solve (rs, xs)

  solve2 :: ([Rule], [[Int]]) -> Int
  solve2 ( _, [])   = 0
  solve2 (rs, x:xs)
    | validateRules x rs = solve (rs, xs)
    | otherwise          = middle (placeAll x [] rs) + solve (rs, xs)

  traverse :: ([Int] -> Int -> [Int] -> a) -> [Int] -> [a]
  traverse _ []     = []
  traverse f (x:xs) = traverse' [] x xs
    where
      traverse' ps c []     = [f ps c []]
      traverse' ps c (n:ns) = f ps c (n:ns) : traverse' (c:ps) n ns

  validateRule :: Rule -> [Int] -> Int -> [Int] -> Bool
  validateRule (Rule a b) pss c nss
    | c == a    = b `notElem` pss
    | c == b    = a `notElem` nss
    | otherwise = True

  validateRules :: [Int] -> [Rule] -> Bool
  validateRules _  []     = True
  validateRules xs (r:rs) = and (traverse (validateRule r) xs) && validateRules xs rs
