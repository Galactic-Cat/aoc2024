module Solution (Grid, Row, solve) where
  import Prelude hiding (Left, Right)
  import qualified Data.Set as Set
  import Data.Maybe (fromMaybe)

  type Coordinate = (Int, Int)
  data Direction = Up | Right | Down | Left
  type Grid = [Row]
  type Plot = Char
  type Row = [Plot]

  coordinates :: Grid -> [Coordinate]
  coordinates g = [(x, y) | x <- [0..(length (head g))], y <- [0..(length g)]]

  directions :: [Direction]
  directions = Up : Right : Down : Left : directions

  findRegions :: Grid -> [Coordinate] -> Set.Set Char -> [(Char, Set.Set Coordinate)]
  findRegions _ []     = []
  findRegions g (c:cs) = 
    case getPlot g c of
      Just '.' -> findRegions g cs
      Just z   ->
        let r@(n, ps) = findRegion g c
        in  r : findRegions (foldr (\c' g' -> setPlot g' c' '.') g ps) cs
      Nothing  -> _

  findRegion :: Grid -> Coordinate -> (Char, Set.Set Coordinate)
  findRegion g c =
    case getPlot g c of
      Just '.' -> ('.', Set.empty)
      Just z   -> (z, findRegion' Set.empty (Set.singleton c))
      Nothing  -> ('.', Set.empty)
    where
      findRegion' :: Set.Set Coordinate -> Set.Set Coordinate -> Set.Set Coordinate
      findRegion' r d
        | Set.null d = r
        | otherwise  = 
          let neighbourhood = Set.foldr (Set.union . Set.fromList . lookAround g) Set.empty d
          in  let d' = Set.difference neighbourhood r
              in  findRegion' (Set.union r neighbourhood) d'

  getPlot :: Grid -> Coordinate -> Maybe Char
  getPlot g (x, y) =
    if   x >= 0 && y >= 0 && x < length (head g) && y < length g
    then Just $ (g !! y) !! x
    else Nothing

  getPos :: Coordinate -> Direction -> Coordinate
  getPos (x, y) Up    = (x,     y - 1)
  getPos (x, y) Right = (x + 1, y)
  getPos (x, y) Down  = (x,     y + 1)
  getPos (x, y) Left  = (x - 1, y)

  lookAround :: Grid -> Coordinate -> [Coordinate]
  lookAround g c@(x, y) = lookAround' (take 4 directions)
    where
      currentName = fromMaybe '.' (getPlot g c)
      lookAround' []     = []
      lookAround' (d:ds) =
        case getPlot g (getPos (x, y) d) of
          Just z  ->
            if   z == currentName
            then getPos (x, y) d : lookAround' ds
            else lookAround' ds
          Nothing -> lookAround' ds

  setPlot :: Grid -> Coordinate -> Char -> Grid
  setPlot []     _      _ = []
  setPlot (r:rs) (x, 0) v = setPlotRow r x : rs
    where
      setPlotRow []     _  = []
      setPlotRow (_:ps) 0  = v : ps
      setPlotRow (p:ps) x' = p : setPlotRow ps (x' - 1)
  setPlot (r:rs) (x, y) v = r : setPlot rs (x, y - 1) v

  solve :: Grid -> IO ()
  solve _ = print "No solution"