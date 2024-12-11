module Parse (parse) where  
  parse :: String -> IO [Int]
  parse path =
    do
      contents <- readFile path
      return . map read . words $ contents