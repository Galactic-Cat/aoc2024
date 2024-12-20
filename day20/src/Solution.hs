module Solution (Coordinate, solve, Space, Walls) where
  import qualified Data.Map as M
  import qualified Data.Set as S

  import qualified PriorityQueue as P

  type Coordinate = (Int, Int)
  type Cost = Int
  type History = M.Map Coordinate (Cost, Coordinate)
  type Queue = P.PriorityQueue Cost Coordinate
  type Space = S.Set Coordinate
  type Walls = S.Set Coordinate

  assertList :: [Maybe a] -> [a]
  assertList []          = []
  assertList (Just x:xs) = x : assertList xs
  assertList (_     :xs) = assertList xs

  cheat :: Coordinate -> History -> Space -> Walls -> [(Coordinate, Int)]
  cheat c h s w = assertList $ concatMap cheat' (filter (`S.member` w) (neighbours c))
    where
      cheat' :: Coordinate -> [Maybe (Coordinate, Int)]
      cheat' wc = map cheat'' (filter (`S.member` s) (neighbours wc))
        where
          cheat'' :: Coordinate -> Maybe (Coordinate, Int)
          cheat'' cwc =
            case (M.lookup c h, M.lookup cwc h) of
              (Nothing    , _           ) -> error "Weird scenario in cheat"
              (_          , Nothing     ) -> Nothing
              (Just (p, _), Just (p', _)) ->
                if   p < p'
                then Just (wc, p' - p)
                else Nothing

  coordinates :: Int -> Int -> Space
  coordinates w 0 = coordinatesLine w 0
  coordinates w h = S.union (coordinatesLine w h) (coordinates w (h - 1))

  coordinatesLine :: Int -> Int -> Space
  coordinatesLine 0 y = S.singleton (0, y)
  coordinatesLine x y = S.insert (x, y) (coordinatesLine (x - 1) y)

  dijkstra :: History -> Queue -> Space -> History
  dijkstra h q s =
    case P.pop q of
      (Nothing, _ ) -> h
      (Just c , q') ->
        let os = dijkstraOptions c s in
        let (h', q'', s') = foldl (dijkstraSingle c) (h, q', s) os in
          dijkstra h' q'' s'

  dijkstraOptions :: Coordinate -> Space -> [Coordinate]
  dijkstraOptions c s = filter (`S.member` s) (neighbours c)

  dijkstraSingle :: Coordinate -> (History, Queue, Space) -> Coordinate -> (History, Queue, Space)
  dijkstraSingle c (h, q, s) c' = case (M.lookup c h, M.lookup c' h) of
    (Nothing    , Nothing     ) -> (M.insert c' (1    , c) h, P.push 1       c' q, S.insert c' s)
    (Nothing    , Just _      ) -> error "Weird situation in dijkstraSingle"
    (Just (p, _), Nothing     ) -> (M.insert c' (p + 1, c) h, P.push (p + 1) c' q, S.insert c' s)
    (Just (p, _), Just (p', _)) ->
      if   p + 1 < p'
      then (M.update (\_ -> Just (p + 1, c)) c' h, P.push (p + 1) c' q, S.insert c' s)
      else (h                                    , q                  , S.insert c' s)

  neighbours :: Coordinate -> [Coordinate]
  neighbours (x, y) = [(x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)]

  solve :: (Space, Walls, Coordinate, Coordinate) -> Int
  solve (s, w, i, _) =
    let h = dijkstra M.empty (P.singletonAsc 0 i) s in
    let cheats = concatMap (\c -> cheat c h s w) (M.keys h) in
      length (filter (> 100) (map snd cheats))