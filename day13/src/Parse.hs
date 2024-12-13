{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Parse (parse) where
  import Text.Regex.TDFA (getAllTextMatches, (=~))

  import Solution (Coordinate, Offset)

  groupIn :: Int -> [a] -> [[a]]
  groupIn _ [] = []
  groupIn x xs = take x xs : groupIn x (drop x xs)
  
  parse :: String -> IO [(Offset, Offset, Coordinate)]
  parse path =
    do
      contents <- readFile path
      return . map parseLines . groupIn 3 . filter (not . null) . lines $ contents

  parseLines :: [String] -> (Offset, Offset, Coordinate)
  parseLines [a, b, p] =
    let [ax, ay] = getAllTextMatches (a =~ "[0-9]+") in
    let [bx, by] = getAllTextMatches (b =~ "[0-9]+") in
    let [px, py] = getAllTextMatches (p =~ "[0-9]+") in
      ((read ax, read ay), (read bx, read by), (read px, read py))
  parseLines _ = error "Huh?"