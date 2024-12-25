module Solution (solve) where
  check :: [[Int]] -> [Int] -> Int
  check []     _ = 0
  check (l:ls) k = 
    if   all (<= 5) (zipWith (+) l k)
    then 1 + check ls k
    else check ls k

  solve :: ([[Int]], [[Int]]) -> Int
  solve (ls, ks) = sum (map (check ls) ks)