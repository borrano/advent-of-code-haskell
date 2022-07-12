module Y20.Day19 (q) where

import Control.Applicative (Alternative (..))
import Data.Attoparsec.ByteString.Char8 (anyChar, char, decimal, endOfLine, isEndOfLine, isSpace, satisfy, sepBy, takeTill, takeWhile)
import Data.ByteString.Char8 qualified as BS
import Data.Foldable
import Data.IntMap.Lazy qualified as M
import Data.List.Extra (splitOn)
import Data.Sequence (Seq ((:<|), (:|>)))
import Data.Sequence qualified as Seq
import Data.Set qualified as S
import Util
import Prelude hiding (takeWhile)

q = do
  (rules, strings) <- runParser parse "./data20/day19.txt"
  let rules' = M.fromList rules
  let q2Rules = M.insert (8) (Or [And [42], And [42, 8]]) $ M.insert 11 (Or [And [42, 31], And [42, 11, 31]]) rules'
  return $ (answer rules' strings, answer (q2Rules) (strings))

data Rule = C Char | And [Int] | Or [Rule] deriving (Show)

parse = do
  r <- rules
  endOfLine *> endOfLine
  lines <- fmap (BS.unpack) <$> many ((takeWhile (/= '\n')) <* char '\n')
  return (r, lines)
  where
    rules = sepBy ((,) <$> (decimal <* char ':') <*> (c <|> o <|> a)) (char '\n')
    c = C <$> token (char '\"' *> anyChar <* char '\"')
    a = And <$> many (token decimal)
    o = Or <$> sepBy a (token (char '|'))
    token p = many (char ' ') *> p <* many (char ' ')

answer rules s = length $ filter id $ (any (null) . go (rules M.! 0)) <$> s
  where
    go (C c) (x : str) | x == c = [str]
    go (Or xs) str = [a | x <- xs, a <- go x str]
    go (And (x : xs)) str = [r | s <- go (rules M.! x) str, r <- go (And xs) s]
    go (And []) str = [str]
    go _ _ = []
