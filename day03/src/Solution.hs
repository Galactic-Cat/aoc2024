module Solution (solve, solve2) where
  import Text.Regex.TDFA ((=~), getAllTextMatches)

  solve :: String -> Int
  solve = sum . map (uncurry (*)) . getNumbers

  solve2 :: String -> Int
  solve2 s = solve2' (getNumbersAndInstructions s) 0 True
    where
      solve2' :: [NAI] -> Int -> Bool -> Int
      solve2' []                 t _     = t
      solve2' (Instruction i:xs) t _     = solve2' xs t i
      solve2' (Numbers a b:xs)   t True  = solve2' xs (t + (a * b)) True
      solve2' (Numbers _ _:xs)   t False = solve2' xs t False

  insRegex :: String
  insRegex = "do(n't)?\\(\\)"

  mulRegex :: String
  mulRegex = "mul\\([0-9]{1,3},[0-9]{1,3}\\)"

  mulInsRegex :: String
  mulInsRegex = mulRegex ++ "|" ++ insRegex

  numRegex :: String
  numRegex = "[0-9]{1,3}"

  getNumbers :: String -> [(Int, Int)]
  getNumbers s = map reader numMatches
    where
      mulMatches :: [String]
      mulMatches = getAllTextMatches $ s =~ mulRegex
      numMatches :: [[String]]
      numMatches = map (getAllTextMatches . (=~ numRegex)) mulMatches
      reader :: [String] -> (Int, Int)
      reader [a, b] = (read a, read b)
      reader _      = error "Reader not reading a 2 element array"

  data NAI = Instruction Bool | Numbers Int Int

  getNumbersAndInstructions :: String -> [NAI]
  getNumbersAndInstructions s = map finder mulInsMatches
    where
      mulInsMatches :: [String]
      mulInsMatches = getAllTextMatches $ s =~ mulInsRegex
      finder :: String -> NAI
      finder z =
        if   z =~ "do"
        then Instruction (z =~ "do\\(\\)")
        else Numbers a b
          where (a, b) = reader (getAllTextMatches $ z =~ numRegex)
      reader :: [String] -> (Int, Int)
      reader [a, b] = (read a, read b)
      reader _      = error "Reader not reading a 2 element array"