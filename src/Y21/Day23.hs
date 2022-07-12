module Y21.Day23 (q) where

import Control.Arrow
import Data.Foldable
import Data.Foldable qualified as M
import Data.Map qualified as M
import Data.PQueue.Prio.Min qualified as P
import Data.Set qualified as S
import Data.Tuple (swap)

q = do
  let (xs, score) = (q1 startingState)
  return (length xs, score)

data Tile = Empty | A | B | C | D deriving (Eq, Ord, Show)

hallways = [(0, 0), (0, 1), (0, 3), (0, 5), (0, 7), (0, 9), (0, 10)]

nslots = 4

rooms = [(b, a) | a <- [2, 4, 6, 8], b <- [1 .. nslots]]

startingState = map
  where
    map = M.fromList $ [(h, Empty) | h <- hallways] ++ zip rooms start
    start = [B, D, D, A, C, C, B, D, B, B, A, C, D, A, C, A]

-- start =   [B, A, C, D, B, C, D, A]
-- start =    [D, D, D, B, D, C, B, A, C, B, A, A, B, A, C, C]

isFinished :: (M.Map (Int, Int) Tile) -> Bool
isFinished map = and [item /= Empty && l == loc item | (x, l) <- rooms, let item = map M.! (x, l)]

point A = 1
point B = 10
point C = 100
point D = 1000

loc D = 8
loc C = 6
loc B = 4
loc A = 2

distance' (a, b) (c, d) = a + c + (abs (d - b))

heuristic :: M.Map (Int, Int) Tile -> Int
heuristic state = sum $ (dist <$> (M.assocs $ M.filter (/= Empty) state))
  where
    dist ((_, x), tile) | loc tile == x = 0
    dist ((x, b), tile) = point tile * distance' (x, b) (1, loc tile)

-- dist ((x, b), tile) = point tile * distance' (x, b) (1, loc tile)

invalidMove state tile pos target = otherTileInSameRoom target tile || any (between pos target) pods
  where
    pods = filter (/= pos) (M.keys $ M.filter (/= Empty) state)
    otherTileInSameRoom (a, x) tile | a /= 0 = loc tile /= x || (a /= nslots && state M.! (a + 1, x) /= tile)
    otherTileInSameRoom _ _ = False
    between (a, b) (c, d) (0, f) = if b < d then f >= b && f <= d else f >= d && f <= b
    between (a, b) (c, d) (e, f) | b == f = e <= a
    between (a, b) (c, d) (e, f) | d == f = e <= c
    between _ _ _ = False

actions state =
  [ (applyAction tile pos target, (tile, pos, target), distance' pos target * point tile)
    | (pos, tile) <- M.assocs $ M.filter (/= Empty) state,
      target <- possibleMoves M.! pos,
      not $ invalidMove state tile pos target
  ]
  where
    applyAction tile pos target = M.insert target tile $ M.insert pos Empty state

possibleMoves =
  M.fromListWith (++) $ [(r1, [r2]) | r1 <- rooms, r2 <- rooms, snd r1 /= snd r2] ++ concat [[(h, [r]), (r, [h])] | h <- hallways, r <- rooms]

q1 state = go S.empty (insert 0 state [] P.empty)
  where
    go set queue =
      case P.deleteFindMin queue of
        ((_, (state, _, _)), newqueue) | S.member state set -> go set newqueue
        ((_, (state, xs, score)), _) | isFinished state -> (xs, score)
        ((_, (state, xs, score)), newqueue) -> go newSet (foldl' (\q (ns, a, score') -> insert (score + score') ns (xs ++ [a]) q) newqueue (actions state))
          where
            newSet = (S.insert state set)
    insert score state x q = P.insert (score + heuristic state) (state, x, score) q
