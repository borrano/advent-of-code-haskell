module Y21.Day17 (q) where

import Data.List (nub)
import Data.List.Extra (maximumOn)
import Data.Maybe

target = ((20, 30), (-10, -5))

problemTarget = ((269, 292), (-68, -44))

inTarget ((xmin, xmax), (ymin, ymax)) (a, b) = a >= xmin && a <= xmax && b >= ymin && b <= ymax

cannotRecover ((xmin, xmax), (ymin, ymax)) x@((curx, cury), (speedx, speedy)) =
  (speedy < 0 && (cury < ymin))
    || (speedx >= 0 && (curx > xmax))
    || (speedx <= 0 && (curx < xmin))

turn ((curx, cury), (speedx, speedy)) = ((curx + speedx, cury + speedy), (speedx', speedy - 1))
  where
    speedx' = if speedx == 0 then 0 else speedx + (-1 * signum speedx)

try target speed =
  let xs = takeWhile (not . cannotRecover target) $ iterate turn ((0, 0), speed)
   in if any (inTarget target . fst) xs then Just $ maximum $ fmap (snd . fst) xs else Nothing

--
q :: IO (Int, Int)
q = do
  let xs = mapMaybe (try problemTarget) $ [(x, y) | x <- [0 .. 292], y <- [-68 .. 68]] -- what is the smallest valid range
  let q1 = maximum xs
  let q2 = length xs
  return (q1, q2)

-- q = try target (9, 0)

-- >>> q
-- ProgressCancelledException
