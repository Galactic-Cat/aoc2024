module Parse (parse) where
  import Text.Regex.TDFA ((=~), getAllTextMatches)

  import Solution (Tile (File, Free))

  parse :: String -> IO [Tile]
  parse path =
    do
      contents <- readFile path
      return $ parseInput (map read (getAllTextMatches (contents =~ "[0-9]")))

  parseInput :: [Int] -> [Tile]
  parseInput iss = parse' iss 0 True
    where
      parse' :: [Int] -> Int -> Bool -> [Tile]
      parse' []     _ _     = []
      parse' (s:ss) i True  = replicate s (File i) ++ parse' ss (i + 1) False
      parse' (s:ss) i False = replicate s Free     ++ parse' ss i       True
