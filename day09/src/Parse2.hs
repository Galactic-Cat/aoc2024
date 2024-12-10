module Parse2 (parse2) where
  import Text.Regex.TDFA ((=~), getAllTextMatches)

  import Solution2 (Tile (File, Free))
  
  parse2 :: String -> IO [Tile]
  parse2 path =
    do 
      contents <- readFile path
      return $ parseInput (map read (getAllTextMatches (contents =~ "[0-9]")))

  parseInput :: [Int] -> [Tile]
  parseInput iss = parse' iss 0 True
    where
      parse' []     _ _     = []
      parse' (s:ss) i True  = File i s : parse' ss (i + 1) False
      parse' (s:ss) i False = Free s   : parse' ss i       True