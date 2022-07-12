module Y20.Day11 (q) where

import Control.Monad.Extra qualified as S
import Control.Monad.Par as P (get, runPar, spawnP)
import Data.Foldable
import Data.Int (Int8)
import Data.List (sort, unfoldr)
import Data.Map qualified as M
import Data.Maybe (mapMaybe)
import Data.Set qualified as S
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as UV
import Data.Word (Word8)
import Debug.Trace (trace)
import Util

untilNoChange f x = go x
  where
    go a = let a' = f a in if a' == a then a else go a'

count = V.sum . V.map (UV.length . UV.elemIndices 1)

go ns k v = runPar $ V.imapM (\i inner -> spawnP $ UV.imap (\j y -> go' i j y) inner) v >>= V.mapM (P.get)
  where
    neigbours' i j = sum $ fmap (get v) $ ns M.! (i, j)
    go' i j 0 | neigbours' i j == 0 = 1
    go' i j 1 | neigbours' i j >= k = 0
    go' _ _ c = c
    get v (a, b) = ((v V.! a) UV.! b)

neigbours1 dots (i, j) =
  [x | i' <- [-1 .. 1], j' <- [-1 .. 1], (i' /= 0 || j' /= 0), let x = (i + i', j + j'), S.notMember x dots]

neigbours2 dots (i, j) =
  [head [(a, b) | k <- [1 ..], let (a, b) = ((i + i' * k), (j + j' * k)), S.notMember (a, b) dots] | i' <- [-1 .. 1], j' <- [-1 .. 1], not (i' == 0 && j' == 0)]

q = do
  xs <- readLinesString "./data20/day11.txt"
  let (len, width) = (length xs, length (head xs))
  let ok (a, b) = a >= 0 && b >= 0 && a < len && b < width

  let dots = S.fromList [(i, j) | (i, xs) <- zip [0 ..] xs, (j, x) <- zip [0 ..] xs, x == '.']
  let empties = [(i, j) | (i, xs) <- zip [0 ..] xs, (j, x) <- zip [0 ..] xs, x == 'L']

  let ns1 = M.fromList [(x, filter ok $ neigbours1 dots x) | x <- empties]
  let ns2 = M.fromList [(x, filter ok $ neigbours2 dots x) | x <- empties]

  let c = (fmap . fmap) (\x -> if x == '.' then -1 else (0 :: Int8)) xs
  let vec = V.fromList (fmap UV.fromList c)

  return (count $ untilNoChange (go ns1 4) vec, count $ untilNoChange (go ns2 5) vec) -- , c $ iterateX go2 vec
