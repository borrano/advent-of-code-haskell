module Y20.Day22 (q) where

import Data.List.Extra
import Data.Map qualified as M
import Data.Set qualified as S
import Util (readLinesString)

q :: IO (Int, Int)
q = do
  lines <- readLinesString "./data20/day22.txt"
  let (p1 : p2 : _) = [read <$> as | (a : as) <- splitOn [""] lines]
  let res = points $ play p1 p2
  let res2 = points $ snd $ play2 S.empty p1 p2

  return (res, res2)

points = sum . zipWith (*) [1 ..] . reverse

play2 set as bs | S.member (as, bs) set = (1, as)
play2 _ [] xs = (2, xs)
play2 _ xs [] = (1, xs)
play2 set xs@(a : as) ys@(b : bs) | a <= (length as) && b <= (length bs) =
  case play2 S.empty (take a as) (take b bs) of
    (1, _) -> play2 (S.insert (xs, ys) set) (as ++ [a, b]) (bs)
    _ -> play2 (S.insert (xs, ys) set) as (bs ++ [b, a])
play2 set xs@(a : as) ys@(b : bs) | a < b = play2 (S.insert (xs, ys) set) as (bs ++ [b, a])
play2 set xs@(a : as) ys@(b : bs) | a > b = play2 (S.insert (xs, ys) set) (as ++ [a, b]) (bs)

play (a : as) (b : bs) | a < b = play as (bs ++ [b, a])
play (a : as) (b : bs) | a > b = play (as ++ [a, b]) (bs)
play [] xs = xs
play xs [] = xs
