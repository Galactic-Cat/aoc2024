{-# LANGUAGE FlexibleInstances #-}
module Solution (solve) where
  import qualified Data.Map as M
  import qualified Data.Set as S
  import Data.List (nub, sort)

  type Vertices = S.Set String
  type Clique = S.Set String
  type Edges = S.Set (String, String)
  type Network = M.Map String (S.Set String)

  assertList :: [Maybe a] -> [a]
  assertList []             = []
  assertList (Just x  : xs) = x : assertList xs
  assertList (Nothing : xs) = assertList xs

  createMap :: [(String, String)] -> Network
  createMap []          = M.empty
  createMap ((a, b):ls) =
    let m = createMap ls in
    case M.lookup a m of
      Just xs -> M.insert a (S.insert b xs) (inverseCreate m)
      Nothing -> M.insert a (S.singleton b) (inverseCreate m)
    where
      inverseCreate m =
        case M.lookup b m of
          Just xs -> M.insert b (S.insert a xs) m
          Nothing -> M.insert b (S.singleton b) m

  cliqueUp :: Vertices -> Edges -> [Clique] -> [Clique]
  cliqueUp _  _  []     = []
  cliqueUp vs es (c:cs) = assertList (map cliqueUp' (S.toList vs)) ++ cliqueUp vs es cs
    where
      cliqueUp' v =
        if   foldr (\v' b -> b && S.member (zheck (v, v')) es) True c
        then Nothing
        else Just $ S.insert v c

  edges :: Network -> Edges
  edges = M.foldrWithKey join S.empty
    where
      join k v s = S.foldr (\v' -> S.insert (zheck (k, v'))) s v

  solve :: [(String, String)] -> Int
  solve i =
    let n  = createMap i in
    let vs = M.keysSet n in
    let es = edges n     in
    let c2 = twoCliques es in
    let c3 = nub $ cliqueUp vs es c2 in
    let ct = filter tFilter c3 in
      length ct

  tFilter :: Clique -> Bool
  tFilter = any ((== 't') . head)

  twoCliques :: Edges -> [Clique]
  twoCliques es = S.toList (S.map (\(a, b) -> S.insert b (S.singleton a)) es)

  zheck :: (String, String) -> (String, String)
  zheck (a, b) =
    case sort [a, b] of
      [a', b'] -> (a', b')
      _        -> error "Could not zheck"