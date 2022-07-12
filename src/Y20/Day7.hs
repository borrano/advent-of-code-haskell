module Y20.Day7 (q) where

import Data.List.Extra (chunksOf, nub)
import Data.Map qualified as M
import Data.Tuple.Extra
import Util (readLines, readLinesString, runParser)
import Prelude hiding (takeWhile)

parseLine :: String -> (String, [(Int, String)])
parseLine line = (unwords [x, y], items)
  where
    (x : y : _ : _ : rest) = words line
    items = concatMap go (chunksOf 4 rest)
    go ("no" : xs) = []
    go (a : b : c : _) = [(read a, unwords [b, c])]

reverseG graph = M.fromListWith (++) [(target, [(c, node)]) | (node, edges) <- M.assocs graph, (c, target) <- edges]

dfs graph node = go [] [(1, node)]
  where
    go visited ((c, node) : xs) =
      let edges = [(c * c', node) | Just xs <- [M.lookup node graph], (c', node) <- xs]
       in go ((c, node) : visited) (edges ++ xs)
    go v [] = v

q = do
  lines <- readLinesString "./data20/day7.txt"
  let graph = M.fromList $ parseLine <$> lines
  let q1 = length $ nub $ fmap snd $ dfs (reverseG graph) "shiny gold"
  let q2 = sum $ fmap fst $ dfs (graph) "shiny gold"
  return (q1 - 1, q2 - 1)
