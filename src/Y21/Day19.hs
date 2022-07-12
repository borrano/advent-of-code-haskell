module Y21.Day19 (q) where

import Control.Monad (guard)
import Data.Either (partitionEithers)
import Data.List (nub, sort)
import Data.List.Extra (splitOn)
import Data.Map.Lazy qualified as M
import Data.Set qualified as S
import Util (readInt)

allrotations = do
  f <- [id, roty, rotz, rotz . rotz, roty . roty . roty, rotz . rotz . rotz]
  s <- [id, rotx, rotx . rotx, rotx . rotx . rotx]
  return (f . s)
  where
    rotx [x, y, z] = [x, (-z), y]
    roty [x, y, z] = [z, y, (-x)]
    rotz [x, y, z] = [(-y), x, z]

test = length $ nub $ ($ [1, 2, 3]) <$> allrotations

distance xs ys = sum $ abs <$> zipWith (-) xs ys

q = do
  xs <- lines <$> readFile "./data21/day19.txt"
  let (a : as) = [([readInt <$> (splitOn "," x) | x <- xs]) | (i, (_ : xs)) <- zip [0 ..] $ splitOn [""] xs]
  let res = q1 as [(a, [0, 0, 0])]
  let q1res = S.size $ S.fromList $ concatMap fst res
  let q2 = maximum [distance a b | a <- snd <$> res, b <- snd <$> res]
  return $ (q1res, q2)

q1 [] as = as
q1 _ [] = []
q1 ys (x : xs) =
  let (l, r) = partitionEithers $ intersections (fst x) <$> ys
   in x : q1 l (xs ++ r)

intersections xs ys = go $ do
  r <- allrotations
  let ys' = r <$> ys
  let m = M.filter (> 2) $ M.fromListWith (+) [(zipWith (-) x y, 1) | x <- take 16 xs, y <- take 16 ys'] -- can fail with a
  case M.keys m of
    [] -> []
    [x] -> return $ (fmap (zipWith (+) x) ys', x)
  where
    go ([(ys')]) = Right (ys')
    go [] = Left (ys)


-- >>> q
-- (438,11985)
