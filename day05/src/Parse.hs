module Parse (parse) where
  import Text.Regex.TDFA ((=~), getAllTextMatches)

  import Solution (Rule (Rule))

  ruleRegex :: String
  ruleRegex = "[0-9]+\\|[0-9]+"
  
  numberRegex :: String
  numberRegex = "[0-9]+"

  parse :: String -> IO ([Rule], [[Int]])
  parse path =
    do
      contents <- readFile path
      return $ separate (map parseLine (lines contents))

  separate :: [Maybe (Either Rule [Int])] -> ([Rule], [[Int]])
  separate []                  = ([], [])
  separate (Nothing:xs)        = separate xs
  separate (Just (Left r):xs)  = let (rs, ns) = separate xs
                                 in  (r:rs, ns)
  separate (Just (Right n):xs) = let (rs, ns) = separate xs
                                 in  (rs, n:ns)

  parseLine :: String -> Maybe (Either Rule [Int])
  parseLine l
    | l =~ ruleRegex   = Just (Left (Rule (head matches) (head $ tail matches)))
    | l =~ numberRegex = Just (Right matches)
    | otherwise        = Nothing
    where
      matches :: [Int]
      matches = map read $ getAllTextMatches (l =~ numberRegex)