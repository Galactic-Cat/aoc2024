module Parse (parse) where
  parse :: String -> IO String
  parse = readFile