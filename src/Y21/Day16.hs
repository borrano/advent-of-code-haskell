module Y21.Day16 (q) where

import Control.Applicative 
import Control.Monad (ap, replicateM, guard)
import Data.List (foldl', unfoldr)
import Data.Tuple (swap)
import Numeric
import Data.ByteString (pack)

packet = "620080001611562C8802118E34"

 
parse ::   String -> ( Packet)
parse = run parsePacket  . toInt
  where
    toInt   = concatMap (toBin . fst . head . readHex . pure)  
    toBin :: Int -> [Int]
    toBin x = reverse $ take 4 $ unfoldr (\a -> if a == 0 then Nothing else Just (swap $ a `divMod` 2)) x ++ repeat 0
    run (Parser f) xs =   snd $  head  $ f xs

q :: IO (Int, Int)
q = do
  xs <- readFile "./data21/day16.txt"
  let packet = parse xs
  return $ ( sum $ q1 $ [packet], q2 packet)

-- >>> q
-- (891,673042777597)
 

q1 (Literal v i : xs) = v : q1 xs
q1 ((Operator v op packets):xs) = v : q1 (packets ++ xs)
q1 _ = []


q2 (Literal v i  ) = i
q2 (Operator v op packets)  = go op (fmap q2 packets) where 
   go 0 xs = sum xs
   go 1 xs = product   xs
   go 2 xs = minimum   xs
   go 3 xs = maximum   xs
   go 5 (x: y: _) =  fromEnum $ x > y  
   go 6 (x: y: _) =fromEnum $ x < y  
   go 7 (x: y: _) = fromEnum $ x == y
   go _ _ = error "cannot happen"  

 

newtype Parser a = Parser {runParser :: [Int] -> [([Int], a)]} deriving (Functor)


data Packet = Literal {version ::Int, num :: Int} | Operator {version ::Int, op :: Int, packets :: [Packet]} deriving (Show)
fromBin = foldl' (\a x -> x + a * 2) 0

parseLiteral =  fromBin <$> do
    id <- fromBin <$> takeN 3
    guard (id == 4)
    literal <- go 
    return literal where 
     go = do
       cont <- fmap(==1) $ fromBin <$> takeN 1
       num <- takeN 4
       if cont then (num++) <$> go else return num 
    
parseOperator ::   Parser (Int, [Packet])
parseOperator   =   do
    t <- fromBin <$> takeN 3
    guard (t /= 4)
    id <- fromBin <$> takeN 1
    xs <- if id == 0 then op0 else op1
    return (t, xs)
     where 
      op1 = do
        n <-  fromBin <$> takeN 11
        xs <- replicateM n ( parsePacket)
        return xs
      op0 = fromBin <$> takeN 15 >>= go
 
      go l = do 
          (a, c) <- consumed (parsePacket)  
          if c == l then return [a] else (a:) <$> (go (l - c))
      
parsePacket :: Parser Packet
parsePacket =  do
  v <- fromBin <$> takeN 3
  k <- (Literal v <$> parseLiteral)    <|>  (uncurry (Operator v) <$> parseOperator   )
  return k

takeN n = Parser $  \xs -> if length xs < n then [] else pure $ swap $ splitAt n  xs
dropZeros = Parser $ \xs -> [(dropWhile (==0) xs, ())]
consumed p = Parser $ \xs -> [ (xs', (a, length xs - length xs')) | (xs', a) <- (runParser p) xs]

instance Applicative Parser where
  pure a = Parser $ \xs -> [(xs, a)]
  (<*>) = ap
instance Alternative Parser where 
  empty = Parser $ \xs -> []
  (Parser f) <|> (Parser g) = Parser $ \xs -> f xs ++ g xs
instance Monad Parser where
  (Parser f) >>= g = Parser $ \xs ->  [a''| (xs', a') <- f xs, a'' <- runParser (g a') xs' ]


 
