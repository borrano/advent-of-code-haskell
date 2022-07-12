module Y21.Day25 (q) where

import Data.List (unfoldr)
import Data.Map qualified as M
import Data.Vector qualified as UV
import Data.Vector qualified as V

iterate' f x =
  x : case f x of
    Nothing -> []
    Just x' -> iterate' f x'

q = do
  xs <- lines <$> readFile "./data21/day25.txt"
  let (h, w) = (length xs, length (head xs))
  let map = M.fromList $ [((i, j), y) | (i, x) <- zip [0 ..] xs, (j, y) <- zip [0 ..] x]
  return (length $ iterate' (answer (h, w)) map)

answer (h, w) map = if null $ xs ++ ys then Nothing else Just map''
  where
    (xs, map') = apply '>' $ M.mapAccumWithKey (go1 map) [] map
    (ys, map'') = apply 'v' $ M.mapAccumWithKey (go2 map') [] map'
    go1 map xs (i, j) '>' =
      let next = if j + 1 >= w then (i, 0) else (i, j + 1)
       in if map M.! next == ('.') then (next : xs, '.') else (xs, '>')
    go1 map xs _ x = (xs, x)
    go2 map xs (i, j) 'v' =
      let next = if i + 1 >= h then (0, j) else (i + 1, j)
       in if map M.! next == ('.') then (next : xs, '.') else (xs, 'v')
    go2 map xs (i, j) x = (xs, x)
    apply c (xs, map') = (xs, foldl (\map i -> M.insert i c map) map' xs)
