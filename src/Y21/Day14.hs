module Y21.Day14 (q) where

import Data.List.Extra (splitOn)
import Data.Map qualified as M

q = do
  xs <- lines <$> readFile "./data21/day14.txt"
  let ([code] : ys : _) = splitOn [""] xs
  let xs' = M.fromList [(x, head y) | i <- ys, let (x : _ : y : _) = (words i)]
  let as = score $ q1 10 code xs'
  let bs = score $ q1 40 code xs'

  return (as, bs) -- (code, xs')

score xs = ((maximum $ xs) - (minimum $ xs))

q1 n code xs = M.unionWith (-) (M.unionsWith (+) $ zipWith (\a b -> map M.! [a, b]) (code) (tail code)) (M.fromList $ (,1) <$> tail (init code))
  where
    tuples = zip (code) (tail code)
    map = (!! n) $ makeMap xs

makeMap :: M.Map String Char -> [M.Map String (M.Map Char Int)]
makeMap xs = iterate go (M.mapWithKey (\k _ -> M.fromListWith (+) $ (,1 :: Int) <$> k) xs)
  where
    go map = M.mapWithKey (\[a, b] v -> M.unionWith (-) (M.unionWith (+) (map M.! [a, v]) (map M.! [v, b])) (M.singleton v 1)) xs

-- >>> q
-- (2192,2360298895775)
