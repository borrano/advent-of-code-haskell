{-# LANGUAGE LambdaCase #-}

module Y21.Day8 (q) where

import Data.Char (ord)
import Data.IntMap qualified as M
import Data.List.Extra
import Data.Set qualified as S
import Util (readLinesString)

-- is this a general solution
-- if there are a single solution to csp like this

-- length, intersection info for each number -> length is unneccessary but it should make the program faster since length comparison is done first
--
makeFrequencyTable :: [S.Set Char] -> [(Int, M.IntMap Int)]
makeFrequencyTable xs = [(S.size x, M.fromListWith (+) [(S.size $ x `S.intersection` y, 1) | y <- xs]) | (i, x) <- zip [(0 :: Int) ..] xs]

table = S.fromList <$> ["abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg"]

parseLine xs = (input, output)
  where
    (input : output : _) = [[S.fromList w | w <- words x] | x <- splitOn "|" xs]

process refMap (input, output) = [i | x <- output, (a, i) <- outMap, x == a]
  where
    inMap = makeFrequencyTable input
    outMap = [(a, num) | (a, x) <- zip input inMap, (num, r) <- zip [0 ..] refMap, x == r]

q = do
  xs <- readLinesString "./data21/day8.txt"
  let lines = (parseLine) <$> xs
  let freqTable = makeFrequencyTable table
  let q1 = process (makeFrequencyTable table) <$> lines
  let q2 = foldl' (\a x -> (10 * a) + x) 0 <$> q1
  return (length $ filter (`elem` [1, 4, 7, 8]) $ concat q1, sum $ q2)
