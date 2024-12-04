module Solution (solve, solve2) where
  import Prelude hiding (Right, Left, getChar, traverse)

  data Direction = Up | UpRight | Right | DownRight | Down | DownLeft | Left | UpLeft

  directions :: [Direction]
  directions = [Up, UpRight, Right, DownRight, Down, DownLeft, Left, UpLeft]

  xmas :: String
  xmas = "XMAS"

  mas :: String
  mas = "MAS"

  getChar :: [String] -> (Int, Int) -> Maybe Char
  getChar g (x, y) =
    if   x >= 0 && y >= 0 && y < length g && x < length (head g)
    then Just $ (g !! y) !! x
    else Nothing

  getPos :: (Int, Int) -> Direction -> (Int, Int)
  getPos (x, y) Up        = (x    , y - 1)
  getPos (x, y) UpRight   = (x + 1, y - 1)
  getPos (x, y) Right     = (x + 1, y    )
  getPos (x, y) DownRight = (x + 1, y + 1)
  getPos (x, y) Down      = (x    , y + 1)
  getPos (x, y) DownLeft  = (x - 1, y + 1)
  getPos (x, y) Left      = (x - 1, y    )
  getPos (x, y) UpLeft    = (x - 1, y - 1)

  search :: String -> [String] -> (Int, Int) -> Direction -> Bool
  search []     _ _ _ = True
  search (l:ls) g c d = checkChar && search ls g (getPos c d) d
    where
      checkChar :: Bool
      checkChar =
        case getChar g c of
          Just q  -> l == q
          Nothing -> False

  solve :: [String] -> Int
  solve g = sum $ traverse (countTrue . lookAround directions) maxX maxY
    where
      lookAround :: [Direction] -> (Int, Int) -> [Bool]
      lookAround []     _ = []
      lookAround (d:ds) c = search xmas g c d : lookAround ds c

      countTrue :: [Bool] -> Int
      countTrue []     = 0
      countTrue (b:bs) = fromEnum b + countTrue bs

      maxX = length (head g) - 1
      maxY = length g - 1

  solve2 :: [String] -> Int
  solve2 g = sum $ traverse (fromEnum . lookAround) maxX maxY
    where
      lookAround :: (Int, Int) -> Bool
      lookAround c
        | getChar g c == Just 'A' =
          (search mas g (getPos c UpLeft) DownRight || search mas g (getPos c DownRight) UpLeft) &&
          (search mas g (getPos c DownLeft) UpRight || search mas g (getPos c UpRight) DownLeft)
        | otherwise               = False

      maxX = length (head g) - 1
      maxY = length g - 1

  traverse :: ((Int, Int) -> a) -> Int -> Int -> [a]
  traverse f mx my = traverse' 0 0
    where
      traverse' x y =
        if   x == mx
        then if   y == my
             then [f (x, y)]
             else f (x, y) : traverse' 0 (y + 1)
        else f (x, y) : traverse' (x + 1) y