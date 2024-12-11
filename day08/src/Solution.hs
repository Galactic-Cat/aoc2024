module Solution (Grid, Row, solve, solve2, Tile (Antenna, Empty)) where
  type Coordinate = (Int, Int)
  type Grid = [Row]
  type Row = [Tile]
  data Tile = Antenna Char | Antinode | Empty

  countAntis :: Grid -> Int
  countAntis []     = 0
  countAntis (r:rs) = countRow r + countAntis rs
    where
      countRow []            = 0
      countRow (Antinode:ts) = 1 + countRow ts
      countRow (_       :ts) = countRow ts

  getAngle :: Coordinate -> Coordinate -> (Int, Int)
  getAngle (ax, ay) (bx, by) = (bx - ax, by - ay)

  getAntis :: Coordinate -> Coordinate -> (Coordinate, Coordinate)
  getAntis a@(ax, ay) b@(bx, by) =
    let (h, v) = getAngle a b
    in  ((ax - h, ay - v), (bx + h, by + v))

  getHarmonicAntis :: Grid -> Coordinate -> Coordinate -> [Coordinate]
  getHarmonicAntis g a b =
    let (h, v) = getAngle a b
    in  traceAntis g a (-h, -v) ++ traceAntis g b (h, v) ++ [a, b]

  getTile :: Grid -> Coordinate -> Maybe Tile
  getTile g (x, y) =
    if   x >= 0 && y >= 0 && x < length (head g) && y < length g
    then Just $ (g !! y) !! x
    else Nothing

  findAntennas :: Grid -> Char -> [Coordinate]
  findAntennas tg l = find tg 0
    where
      find []       _ = []
      find (ts:tss) y = findRow ts (0, y) ++ find tss (y + 1)
      findRow []              _      = []
      findRow (Antenna l':ts) (x, y)
        | l' == l   = (x, y) : findRow ts (x + 1, y)
        | otherwise = findRow ts (x + 1, y)
      findRow (_         :ts) (x, y) = findRow ts (x + 1, y)

  setTile :: Grid -> Coordinate -> Tile -> Grid
  setTile g (x, y) t =
    case splitGrid g y of
      (_ , [])   -> g
      (gp, r:rs) ->
        case splitRow r x of
          (_ , [])   -> g
          (rp, _:ts) -> gp ++ ((rp ++ (t : ts)) : rs)

  splitGrid :: Grid -> Int -> (Grid, Grid)
  splitGrid []     _ = ([], [])
  splitGrid g      0 = ([], g)
  splitGrid (r:rs) y =
    let (pr, nr) = splitGrid rs (y - 1)
    in  (r : pr, nr)

  splitRow :: Row -> Int -> (Row, Row)
  splitRow []     _ = ([], [])
  splitRow r      0 = ([], r)
  splitRow (t:ts) x =
    let (pt, nt) = splitRow ts (x - 1)
    in  (t : pt, nt)

  solve :: (Grid, [Char]) -> Int
  solve (g, lss) = countAntis $ solve' g lss
    where
      solve' :: Grid -> [Char] -> Grid
      solve' ng []     = ng
      solve' ng (l:ls) = solve' (solveAntennas ng antennas) ls
        where
          antennas = findAntennas g l

  solve2 :: (Grid, [Char]) -> Int
  solve2 (g, lss) = countAntis $ solve2' g lss
    where
      solve2' :: Grid -> [Char] -> Grid
      solve2' ng []     = ng
      solve2' ng (l:ls) = solve2' (solveHarmonicAntennas ng antennas) ls
        where
          antennas = findAntennas g l

  solveAntenna :: Grid -> Coordinate -> [Coordinate] -> Grid
  solveAntenna g _ []     = g
  solveAntenna g a (b:bs) =
    let (n1, n2) = getAntis a b
    in  solveAntenna (setTile (setTile g n2 Antinode) n1 Antinode) a bs

  solveAntennas :: Grid -> [Coordinate] -> Grid
  solveAntennas g []     = g
  solveAntennas g [_]    = g  
  solveAntennas g (c:cs) = solveAntennas (solveAntenna g c cs) cs

  solveHarmonicAntenna :: Grid -> Coordinate -> [Coordinate] -> Grid
  solveHarmonicAntenna g _ []     = g
  solveHarmonicAntenna g a (b:bs) =
    let ns = getHarmonicAntis g a b
    in  solveHarmonicAntenna (foldr (\x g' -> setTile g' x Antinode) g ns) a bs

  solveHarmonicAntennas :: Grid -> [Coordinate] -> Grid
  solveHarmonicAntennas g []     = g
  solveHarmonicAntennas g [_]    = g
  solveHarmonicAntennas g (c:cs) = solveHarmonicAntennas (solveHarmonicAntenna g c cs) cs

  traceAntis :: Grid -> Coordinate -> (Int, Int) -> [Coordinate]
  traceAntis g (x, y) a@(h, v) =
    case getTile g (x + h, y + v) of
      Just _  -> c' : traceAntis g c' a
      Nothing -> []
    where
      c' = (x + h, y + v)