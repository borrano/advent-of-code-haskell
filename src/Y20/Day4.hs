module Y20.Day4 (q) where

import Control.Applicative
import Data.ByteString.Char8 qualified as BS
import Data.List.Extra (chunksOf, split, splitOn)
import Debug.Trace (trace)
import Text.Read (readMaybe)
import Util (readLines, runParser)
import Prelude hiding (takeWhile)

parseFile lines =
  [ [(header, BS.unpack field) | line <- passport, w <- BS.words (line), let (header : field : _) = BS.split (':') w]
    | passport <- split BS.null lines
  ]

q = do
  passports <- parseFile <$> readLines "./data20/day4.txt"
  let ps = fmap (filter ((/= "cid") . fst)) passports
  return (q1 ps, q2 ps)

q1 = length . filter (validPassport)

q2 = length . filter (validPassport) . fmap (filter (uncurry validField))

validPassport xs = length xs == 7

between a b x = maybe False (\i -> i >= a && i <= b) $ readMaybe x

validField :: BS.ByteString -> String -> Bool
validField "byr" a = between 1920 2002 a
validField "iyr" a = between 2010 2020 a
validField "eyr" a = between 2020 2030 a
validField "hgt" (a : b : "in") = between 59 76 [a, b]
validField "hgt" (a : b : c : "cm") = between 150 193 [a, b, c]
validField "hcl" ('#' : xs) = all (\c -> (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')) xs
validField "ecl" a = a `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
validField "pid" a = length a == 9
validField _ _ = False
