module Solution (solve, Rule (Rule)) where
  import Debug.Trace (trace)

  data Rule = Rule Int Int

  middle :: [a] -> a
  middle xs = xs !! (length xs `div` 2)

  validateRules :: [Int] -> [Rule] -> Bool
  validateRules []     _  = True
  validateRules (x:xs) rs = foldr ((&&) . validateOrder x xs) True rs

  validateOrder :: Int -> [Int] -> Rule -> Bool
  validateOrder _ []     _            = True
  validateOrder z (x:xs) r@(Rule a b)
    | b == z && a == x = trace (show a ++ " should appear before " ++ show b) False
    | b == z           = validateOrder z xs r
    | otherwise        = True

  solve :: ([Rule], [[Int]]) -> Int
  solve (_ , [])   = 0
  solve (rs, x:xs)
    | validateRules x rs = middle x + solve (rs, xs)
    | otherwise          = solve (rs, xs)
