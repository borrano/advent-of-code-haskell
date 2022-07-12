{-# LANGUAGE LambdaCase #-}

module Y20.Day18 (q) where

import Control.Applicative (Alternative (many), (<|>))
import Data.Attoparsec.ByteString.Char8 (char, decimal, isSpace, satisfy, sepBy)
import Data.ByteString qualified as BS
import Data.Either (isLeft)
import Data.Either.Extra (fromEither)
import Data.IntMap qualified as M
import Data.IntSet qualified as S
import Util (readLinesString, runParser, runParser')

skipWhiteSpaces c = many (char ' ') *> char c <* many (char ' ')

add = (skipWhiteSpaces '+' *> pure (+))

mul = (skipWhiteSpaces '*' *> pure (*))

factor e = skipWhiteSpaces ('(') *> e <* skipWhiteSpaces (')') <|> decimal

chainl p op = (p >>= rest)
  where
    rest x = ((op <*> pure x <*> p) >>= rest) <|> return x

expr1 = chainl (factor expr1) (add <|> mul)

expr2 = chainl (chainl (factor expr2) add) (mul)

e parser xs = sum $ runParser' (sepBy parser (char '\n')) xs

q :: IO (Int, Int)
q = do
  xs <- BS.readFile "./data20/day18.txt"
  return (e expr1 xs, e expr2 xs)
