{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE GADTs #-}

module PriorityQueue (emptyAsc, emptyDsc, peek, pop, PriorityQueue, push) where
  data PriorityQueue k v where
    AscQueue :: Ord k => [(k, v)] -> PriorityQueue k v
    DscQueue :: Ord k => [(k, v)] -> PriorityQueue k v

  emptyAsc :: Ord k => PriorityQueue k v
  emptyAsc = AscQueue []

  emptyDsc :: Ord k => PriorityQueue k v
  emptyDsc = DscQueue []

  peek :: PriorityQueue k v -> Maybe v
  peek (AscQueue []        ) = Nothing
  peek (AscQueue ((_, v):_)) = Just v
  peek (DscQueue []        ) = Nothing
  peek (DscQueue ((_, v):_)) = Just v

  pop :: PriorityQueue k v -> (Maybe v, PriorityQueue k v)
  pop q@(AscQueue []         ) = (Nothing, q)
  pop q@(DscQueue []         ) = (Nothing, q)
  pop   (AscQueue ((_, v):qs)) = (Just v , AscQueue qs)
  pop   (DscQueue ((_, v):qs)) = (Just v , DscQueue qs)

  push :: Ord k => k -> v -> PriorityQueue k v -> PriorityQueue k v
  push k v (AscQueue [])            = AscQueue [(k, v)]
  push k v (DscQueue [])            = DscQueue [(k, v)]
  push k v (AscQueue ((k', v'):vs))
    | k <= k'   = AscQueue ((k, v) : (k', v') : vs)
    | otherwise =
      let (AscQueue vs') = push k v (AscQueue vs) in
        AscQueue ((k', v') : vs')
  push k v (DscQueue ((k', v'):vs))
    | k >= k'   = DscQueue ((k, v) : (k', v') : vs)
    | otherwise =
      let (DscQueue vs') = push k v (DscQueue vs) in
        DscQueue ((k', v') : vs')