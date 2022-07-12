module Y21.Day22 (q) where

import Data.List.Extra (foldl', nub, splitOn)
import Util (readInt)

q = do
  xs <- lines <$> readFile "./data21/day22.txt"
  let cubes@((cube1) : cube2 : _) = [(a == "on", parse rest) | l <- xs, let (a : rest : _) = words l]
  let cubes' = foldl add [] cubes
  let q2 = sum $ area <$> cubes'
  let q1 = q2 - (sum $ area <$> add cubes' (False, [(-50, 50), (-50, 50), (-50, 50)]))
  return (q1, q2)

parse str = [x, y, z]
  where
    [x, y, z] = fmap toTuple $ fmap (readInt) <$> splitOn ".." <$> drop 2 <$> splitOn "," str
    toTuple [a, b] = (a, b)

different cube1 cube2 = or $ zipWith dif cube1 cube2
  where
    dif (amin, amax) (bmin, bmax) = amin > bmax || bmin > amax

add [] (True, cube) = [cube]
add (x : xs) (b, cube) | different x cube = x : add xs (b, cube)
add (cube1 : xs) (b, cube2) = remove cube1 cube2 ++ add xs (b, cube2)
add xs _ = xs

remove cube1@[(xmin1, xmax1), (ymin1, ymax1), (zmin1, zmax1)] cube2 =
  filter (all (\(a, b) -> a <= b)) $
    [ [(xmax2 + 1, xmax1), (ymin1, ymax1), (zmin1, zmax1)],
      [(xmin1, xmin2 - 1), (ymin1, ymax1), (zmin1, zmax1)],
      [(xmin2, xmax2), (ymax2 + 1, ymax1), (zmin1, zmax1)],
      [(xmin2, xmax2), (ymin1, ymin2 - 1), (zmin1, zmax1)],
      [(xmin2, xmax2), (ymin2, ymax2), (zmax2 + 1, zmax1)],
      [(xmin2, xmax2), (ymin2, ymax2), (zmin1, zmin2 - 1)]
    ]
  where
    [(xmin2, xmax2), (ymin2, ymax2), (zmin2, zmax2)] = zipWith intersects cube1 cube2
    intersects (amin, amax) (bmin, bmax) = (max amin bmin, min amax bmax)

area = product . fmap (\(min, max) -> max - min + 1)

-- >>> q
-- (582644,1263804707062415)
