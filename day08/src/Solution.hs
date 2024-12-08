module Solution (Grid, Row, solve, Tile (Antenna, Empty)) where
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

  solveAntenna :: Grid -> Coordinate -> [Coordinate] -> Grid
  solveAntenna g _ []     = g
  solveAntenna g a (b:bs) =
    let (n1, n2) = getAntis a b
    in  solveAntenna (setTile (setTile g n2 Antinode) n1 Antinode) a bs

  solveAntennas :: Grid -> [Coordinate] -> Grid
  solveAntennas g []     = g
  solveAntennas g [_]    = g  
  solveAntennas g (c:cs) = solveAntennas (solveAntenna g c cs) cs
