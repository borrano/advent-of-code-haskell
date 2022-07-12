module Y21.Day20 (q) where

import Data.IntMap qualified as M
import Data.List.Extra (splitOn)
import Data.Maybe (fromMaybe)
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as UV

q = do
  xs <- lines <$> readFile "./data21/day20.txt"

  let ([code] : xss : _) = splitOn [""] xs
  let codeMap = M.fromList $ zip [0 ..] $ fmap (\x -> if x == '#' then 1 else 0) code
  let vec = V.fromList [UV.fromList [if x == '#' then 1 else 0 | x <- xs] | xs <- xss]
  let q1res n = V.sum . V.map (UV.sum) $ snd $ (!! n) $ iterate (q1 codeMap) (0, vec)
  return $ (q1res 50)

q1 map (i, vec) = (i + 1, V.fromList [UV.fromList [go (x, y) | y <- [-1 .. UV.length (vec V.! 0)]] | x <- [-1 .. (V.length vec)]])
  where
    get (a, b) = fromMaybe (i `mod` 2) $ (vec V.!? a >>= (UV.!? b))
    go (a, b) = map M.! (toBin $ get <$> [(a + x, b + y) | x <- [-1 .. 1], y <- [-1 .. 1]])
    toBin = foldl (\a x -> a * 2 + x) 0

-- >>> q
-- ProgressCancelledException
