module Parse (parse) where
  parse :: String -> IO ([[Int]], [[Int]])
  parse path =
    do
      contents <- readFile path
      return . parseLines . lines $ contents

  parseLines :: [String] -> ([[Int]], [[Int]])
  parseLines []      = ([], [])
  parseLines ("":ls) = parseLines ls
  parseLines ls      =
    let (ls', ks') = parseLines (drop 7 ls)     in
    let block      = take 7 ls                  in
    let lock       = head (head block) == '#'   in
    let res        = parseBlock block           in
      if   lock
      then (res : ls', ks'      )
      else (ls'      , res : ks')

  parseBlock :: [String] -> [Int]
  parseBlock = foldl (\a b -> zipWith (+) a (map check b)) (replicate 5 (-1))
    where
      check '#' = 1
      check '.' = 0
      check c   = error $ "Couldn't parse char: " ++ [c]