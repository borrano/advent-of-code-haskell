{-# LANGUAGE LambdaCase #-}

module Y20.Day8 (q) where

import Data.Either (isLeft)
import Data.Either.Extra (fromEither)
import Data.IntMap qualified as M
import Data.IntSet qualified as S
import Util (readLinesString)

-- for second problem - complexity O(number of ops * number of jumps)
-- faster solution may be: find reachable vertices from end of the node (in reversed graph)
-- for any of them check (instr number - 1) is a jump and reachable from first op

data Op = Nop {next :: Int} | Jump {next :: Int} | Acc {value :: Int, next :: Int}

parseLines :: [String] -> (M.IntMap Op)
parseLines xs = M.fromList [(i, parse i (words line)) | (i, line) <- zip [0 ..] xs]
  where
    parse :: Int -> [String] -> (Op)
    parse i ("nop" : xs) = Nop (i + 1)
    parse i ("jmp" : x : xs) = Jump (i + parseNum x)
    parse i ("acc" : x : xs) = Acc (parseNum x) (i + 1)
    parseNum ('+' : xs) = read xs
    parseNum ('-' : xs) = -read xs

execute ops = go False 0 S.empty 0
  where
    go _ acc visited cur | S.member cur visited = [Left acc]
    go _ acc visited cur | M.notMember cur ops = [Right acc]
    go changed acc visited cur =
      let visited' = S.insert cur visited
       in case ops M.! cur of
            Jump next | not changed -> go changed acc visited' next ++ go True acc visited' (cur + 1)
            Acc v next -> go changed (acc + v) visited' next
            x -> go changed acc visited' (next x)

q = do
  xs <- parseLines <$> readLinesString "./data20/day8.txt"
  return ((fromEither . head . execute) xs, (fromEither . head . dropWhile (isLeft) . execute) xs)
