module Solution (solve) where
  import qualified Data.Map as M
  import Debug.Trace (trace)
  import Prelude hiding (Left, Right)
  import Text.Regex.TDFA ((=~))

  type Coordinate = (Int, Int)
  data DChar = DAccept | Down | Left | Right | Up
    deriving (Eq, Ord)
  data NChar = NAccept | Eight | Five | Four | Nine | One | Seven | Six | Three | Two | Zero
    deriving (Eq, Ord, Show)
  data PanelType = DirectionPanel | NumericPanel

  instance Show DChar where
    show DAccept = "A"
    show Down    = "v"
    show Left    = "<"
    show Right   = ">"
    show Up      = "^"

  dCharMap :: M.Map DChar Coordinate
  dCharMap = M.fromList [(Up, (1, 0)), (DAccept, (2, 0)), (Left, (0, 1)), (Down, (1, 1)), (Right, (2, 1))]

  concatMapAdjacent :: (a -> a -> [b]) -> [a] -> [b]
  concatMapAdjacent _ []         = []
  concatMapAdjacent _ [_]        = []
  concatMapAdjacent f (a1:a2:as) = f a1 a2 ++ concatMapAdjacent f (a2:as)

  move :: Coordinate -> Coordinate -> PanelType -> [DChar]
  move (ax, ay) (bx, by) DirectionPanel =
    if   ax > bx && not (ay == 0 && bx == 0)
    then moveHorizontal ax bx ++ moveVertical ay by ++ [DAccept]
    else moveVertical ay by ++ moveHorizontal ax bx ++ [DAccept]
  move (ax, ay) (bx, by) NumericPanel   =
    if   ax > bx && not (ay == 3 && bx == 0)
    then moveHorizontal ax bx ++ moveVertical ay by ++ [DAccept]
    else moveVertical ay by ++ moveHorizontal ax bx ++ [DAccept]

  moveDeep :: [NChar] -> Int -> [DChar]
  moveDeep ns 0 = concatMapAdjacent moveNumeric (NAccept : ns)
  moveDeep ns x =
    let ds = moveDeep ns (x - 1) in
      concatMapAdjacent moveDirectional (DAccept : ds)

  moveDirectional :: DChar -> DChar -> [DChar]
  moveDirectional a b =
    case (M.lookup a dCharMap, M.lookup b dCharMap) of
      (Nothing, _      ) -> error $ "Value " ++ show a ++ " missing from directional map"
      (_      , Nothing) -> error $ "Value " ++ show b ++ " missing from directional map"
      (Just a', Just b') -> move a' b' DirectionPanel

  moveHorizontal :: Int -> Int -> [DChar]
  moveHorizontal ax bx
    | ax > bx   = replicate (ax - bx) Left
    | ax < bx   = replicate (bx - ax) Right
    | otherwise = []

  moveNumeric :: NChar -> NChar -> [DChar]
  moveNumeric a b =
    case (M.lookup a nCharMap, M.lookup b nCharMap) of
      (Nothing, _      ) -> error $ "Value " ++ show a ++ " missing from numeric map"
      (_      , Nothing) -> error $ "Value " ++ show b ++ " missing from numeric map"
      (Just a', Just b') -> move a' b' NumericPanel

  moveVertical :: Int -> Int -> [DChar]
  moveVertical ay by
    | ay > by   = replicate (ay - by) Up
    | by > ay   = replicate (by - ay) Down
    | otherwise = []

  nCharMap :: M.Map NChar Coordinate
  nCharMap = M.fromList [(Seven, (0, 0)), (Eight, (1, 0)), (Nine, (2, 0)), (Four, (0, 1)), (Five, (1, 1)), (Six, (2, 1)), (One, (0, 2)), (Two, (1, 2)), (Three, (2, 2)), (Zero, (1, 3)), (NAccept, (2, 3))]

  solve :: [String] -> Int
  solve []     = 0
  solve (s:ss) =
    let ns = translate s in
    let sq = moveDeep ns 25 in
    let n  = read (s =~ "[0-9]+") in
      n * length sq + solve ss

  translate :: String -> [NChar]
  translate []       = []
  translate ('0':cs) = Zero    : translate cs
  translate ('1':cs) = One     : translate cs
  translate ('2':cs) = Two     : translate cs
  translate ('3':cs) = Three   : translate cs
  translate ('4':cs) = Four    : translate cs
  translate ('5':cs) = Five    : translate cs
  translate ('6':cs) = Six     : translate cs
  translate ('7':cs) = Seven   : translate cs
  translate ('8':cs) = Eight   : translate cs
  translate ('9':cs) = Nine    : translate cs
  translate ('A':cs) = NAccept : translate cs
  translate (c  :_ ) = error $ "Can't translate character " ++ [c]

  za :: Show a => [a] -> [a]
  za x = trace (concatMap show x) x