module Y20.Day20 (q) where

import Control.Monad (guard)
import Data.List.Extra
import Data.Map qualified as M
import Data.Set qualified as S
import Util (readLinesString)

parseLine :: IO [(Int, [String])]
parseLine = do
  lines <- readLinesString "./data20/day20.txt"
  return $ [(read $ init (words h !! 1), rest) | (h : rest) <- splitOn [""] lines]

edges xs = [top xs, bottom xs, right xs, left xs]

top = head

bottom = last

right = fmap last

left = fmap head

graph tiles = M.map (nub) $ M.fromListWith (++) $ concat [[(num, [num1]), (num1, [num])] | (tile, [num, num1]) <- M.assocs xs]
  where
    xs = M.fromListWith (++) $ concat [[(side, [i]), (reverse side, [i])] | (i, sides) <- tiles, side <- edges sides]

combinations xs = xss ++ (horizontal <$> xss)
  where
    xss = take 4 $ iterate rotate xs
    rotate = transpose . map reverse
    horizontal xs = reverse xs

matchHorizontal xs ys = [ys' | ys' <- combinations ys, right xs == left ys']

matchVertical xs ys = [ys' | ys' <- combinations ys, bottom xs == top ys']

process = concat . fmap (fmap concat . transpose)

test :: [[[String]]]
test = [[["12", "45"], ["78", "90"]], [["aa", "bb"], ["cc", "dd"]]]

size = 12 -- 3

start = 2719 -- 1951

crosses = length . filter (== '#') . concat

answer1 g i = [(crosses a - 15 * x) | a <- combinations $ answer g i, let x = zipx a, x > 0]

zipx (x : y : z : rest) = (monster x y z) + zipx (y : z : rest)
zipx _ = 0

removeBorder xs = fmap init $ fmap tail $ tail $ init xs

answer graph items = process $ (fmap . fmap) removeBorder $ head [a | xs <- combinations (items M.! start), a <- go size (start, xs)]
  where
    go x (n, elems) = do
      firstLine <- take 1 $ horizontal (n, elems)
      guard (length firstLine == (size - 1))
      if x == 1
        then [[(elems : firstLine)]]
        else do
          next <- vertical (n, elems)
          rest <- go (x - 1) next
          [(elems : firstLine) : rest]

    horizontal (n, elems) = [(x) : rest | neigbour <- graph M.! n, x <- matchHorizontal elems (items M.! neigbour), rest <- horizontal (neigbour, x)] ++ [[]]
    vertical (n, elems) = [(neigbour, x) | neigbour <- graph M.! n, x <- matchVertical elems (items M.! neigbour)]

monster a b c | length a < 19 || length b < 20 || length c < 17 = 0
monster
  a
  b@('#' : _ : _ : _ : _ : '#' : '#' : _ : _ : _ : _ : '#' : '#' : _ : _ : _ : _ : '#' : '#' : '#' : _)
  c@(_ : '#' : _ : _ : '#' : _ : _ : '#' : _ : _ : '#' : _ : _ : '#' : _ : _ : '#' : _)
    | a !! 18 == '#' = 1 + monster (tail a) (tail b) (tail c)
monster a b c = monster (tail a) (tail b) (tail c)

q = do
  xs <- parseLine
  let graph' = graph xs
  let items' = M.fromList xs
  let corners = M.filter ((== 2) . length) graph'
  return $ (product $ M.keys corners, head $ answer1 graph' items')
