module Util where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8 (char, decimal, parseOnly)
import Data.ByteString.Char8 qualified as BS
import Data.Either (fromRight)
import Data.List (sort)

readNums :: String -> IO [Int]
readNums fName = runParser (many ((decimal <* char '\n') <|> (decimal))) fName

runParser parser fName = do
  xs <- BS.readFile fName
  let Right x = parseOnly parser xs
  return x

runParser' parser xs = fromRight undefined $ parseOnly parser xs

readLines :: String -> IO [BS.ByteString]
readLines fName = do
  xs <- BS.readFile fName
  return $ BS.lines xs

readLinesString :: String -> IO [String]
readLinesString fName = do
  xs <- readFile fName
  return $ lines xs

------

myIterateM_ :: Monad m => Int -> (a -> m a) -> a -> m ()
myIterateM_ 0 _ a = return ()
myIterateM_ n f a = f a >>= myIterateM_ (n - 1) f

addTuple (a, b) (c, d) = ((a + c), (b + d))

readInt :: String -> Int
readInt = read

median xs = (sort xs) !! (length xs `div` 2)
