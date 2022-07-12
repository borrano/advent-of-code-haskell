module Y21.Day12 (q) where

import Control.Monad.Extra qualified as S
import Data.Char (isLower, isUpper)
import Data.List.Extra (nub, splitOn, trim)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Set qualified as S

q = do
  xs <- (lines) <$> readFile "./data21/day12.txt"
  let map = M.fromListWith (++) $ concat [[(a, [b]), (b, [a])] | l <- xs, let (a : b : _) = trim <$> splitOn "-" l]
  let q1 = paths visit1 S.empty map
  let q2 = paths visit2 (False, S.empty) map
  return (q1, q2)

visit1 x s | any isUpper x = Just s
visit1 x s = if S.notMember x s then Just (S.insert x s) else Nothing

visit2 x s | any isUpper x = Just s
visit2 x (b, s) | S.notMember x s = Just (b, S.insert x s)
visit2 "start" _ = Nothing
visit2 x (True, s) = Nothing
visit2 x (b, s) = Just (True, S.insert x s)

paths f a map = go a "start"
  where
    go s "end" = 1
    go s x | (Just a') <- f x s = sum $ (go a') <$> (fromMaybe [] $ M.lookup x map)
    go s _ = 0

-- >>> q
-- (4773,116985)
