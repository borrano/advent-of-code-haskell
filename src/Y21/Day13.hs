module Y21.Day13 (q) where

import Data.List.Extra
import Data.Set qualified as S

q :: IO (Int, String)
q = do
  xs <- lines <$> readFile "./data21/day13.txt"
  let (points : instructions : _) = splitOn [""] xs
  let points' = [(read a, read b) | p <- points, let (a : b : _) = splitOn "," p]
  let instructions' = [(x, read a) | i <- instructions, let (x : a : _) = splitOn "=" (words i !! 2)]
  let xs = S.size $ q1 points' (take 1 instructions')
  let ys = printCode $ q1 points' instructions'
  -- putStrLn ys
  return (xs, "ALREKFKU")

printCode xs = unlines [concat [if S.member (y, x) xs then " x " else " . " | y <- [0 .. 50]] | x <- [0 .. 5]]

q1 :: [(Int, Int)] -> [(String, Int)] -> S.Set (Int, Int)
q1 xs ys = foldl go (S.fromList xs) ys
  where
    go xs ("x", n) = S.map (\(a, b) -> if a > n then (2 * n - a, b) else (a, b)) xs
    go xs ("y", n) = S.map (\(a, b) -> if b > n then (a, 2 * n - b) else (a, b)) xs

-- >>> q
-- (618,"ALREKFKU")
