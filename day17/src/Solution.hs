module Solution (solve, solve2, Tape) where
  import Data.Bits (shiftL, shiftR, xor)
  
  data State = State Int Int Int Int [Int]
  type Tape = [Trit]
  type Trit = Int

  adv :: State -> Trit -> State
  adv s@(State i a b c o) x = State (i + 2) (a `div` 2^combo s x) b c o

  bdv :: State -> Trit -> State
  bdv s@(State i a _ c o) x = State (i + 2) a (a `div` round ((2 :: Double) ** fromIntegral (combo s x))) c o

  bst :: State -> Trit -> State
  bst s@(State i a _ c o) x = State (i + 2) a (combo s x `mod` 8) c o

  bxl :: State -> Trit -> State
  bxl (State i a b c o) x = State (i + 2) a (xor b x) c o

  bxc :: State -> Trit -> State
  bxc (State i a b c o) _ = State (i + 2) a (xor b c) c o

  cdv :: State -> Trit -> State
  cdv s@(State i a b _ o) x = State (i + 2) a b (a `div` round ((2 :: Double) ** fromIntegral (combo s x))) o

  combo :: State -> Trit -> Int
  combo _                 0 = 0
  combo _                 1 = 1
  combo _                 2 = 2
  combo _                 3 = 3
  combo (State _ a _ _ _) 4 = a
  combo (State _ _ b _ _) 5 = b
  combo (State _ _ _ c _) 6 = c
  combo _                 7 = error "Reserved combo operator 7"
  combo _                 x = error $ "[combo] Trit " ++ show x ++ " out of range"

  jnz :: State -> Trit -> State
  jnz (State i 0 b c o) _ = State (i + 2) 0 b c o
  jnz (State _ a b c o) x = State x       a b c o

  out :: State -> Trit -> State
  out s@(State i a b c o) x = State (i + 2) a b c (combo s x `mod` 8 : o)

  run :: State -> Tape -> State
  run s@(State i _ _ _ _) t
    | i > length t - 1 = s
    | otherwise        =
    run (op s (t !! (i + 1))) t
    where
      op :: State -> Trit -> State
      op = case t !! i of
        0 -> adv
        1 -> bxl
        2 -> bst
        3 -> jnz
        4 -> bxc
        5 -> out
        6 -> bdv
        7 -> cdv
        x -> error $ "Op code " ++ show x ++ " out of range"

  solve :: (Int, Int, Int, Tape) -> String
  solve (a, b, c, t) =
    let (State _ _ _ _ o) = run (State 0 a b c []) t
    in  show $ reverse o

  solve2 :: (Int, Int, Int, Tape) -> Int
  solve2 (_, b, c, t) = solve2' 0 b c t (length t)
  
  solve2' :: Int -> Int -> Int -> Tape -> Int -> Int
  solve2' a _ _ _ 0 = shiftR a 3
  solve2' a b c t l =
    let (State _ _ _ _ o) = run (State 0 a b c []) t in
    if   all (uncurry (==)) (zip (drop (l - 1) t) (reverse o))
    then solve2' (shiftL a 3) b c t (l - 1)
    else solve2' (a + 1)      b c t l
