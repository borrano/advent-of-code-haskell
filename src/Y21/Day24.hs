module Y21.Day24 (q) where

import Control.Monad.State.Strict
import Data.Map qualified as M

type Interval a b = (a, b)

class Get a where
  mk :: Int -> a

instance Get Int where
  mk = id

instance Get (Int, Int) where
  mk s = (s, s)

data Op = Add | Mul | Mod | Div | Eql deriving (Eq, Show)

getEnv env str =
  case M.lookup str env of
    Nothing -> mk (read str)
    Just x -> x

putEnv env k v = (M.insert k v env)

operation Add (!a, !b) (!c, !d) = ((a + c), (b + d))
operation Mul (a, b) (c, d)
  | a >= 0 && c >= 0 = ((a * c), (b * d))
  | b <= 0 && d <= 0 = ((b * d), (a * c))
  | a >= 0 && d <= 0 = ((a * d), (b * c))
  | b <= 0 && c >= 0 = ((b * c), (a * d))
operation Div (a, b) (c, d)
  | c > 0 = ((a `quot` d), (b `quot` c))
  | d < 0 = ((a `quot` c), (b `quot` d))
operation Mod (a, b) (c, d) | c > 0 = (0, (d - 1))
operation Eql (a, b) (c, d)
  | b < c = (0, 0)
  | a > d = (0, 0)
  | a == b && a == c && a == d = (1, 1)
  | otherwise = (0, 1)

go' env op var1 var2 = parse' $ putEnv env var1 $ operation op (getEnv env var1) (getEnv env var2)

parse' env (("add" : var1 : var2 : _) : xs) = go' env Add var1 var2 xs
parse' env (("mul" : var1 : var2 : _) : xs) = go' env Mul var1 var2 xs
parse' env (("mod" : var1 : var2 : _) : xs) = go' env Mod var1 var2 xs
parse' env (("div" : var1 : var2 : _) : xs) = go' env Div var1 var2 xs
parse' env (("eql" : var1 : var2 : _) : xs) = go' env Eql var1 var2 xs
parse' env (("inp" : var1) : xs) = parse' (putEnv env "w" (1, 9)) xs
parse' env [] = let (imin, imax) = getEnv env "z" in (imin <= 0 && imax >= 0)

go code env f var1 var2 = parse code $ putEnv env var1 $ f (getEnv env var1) (getEnv env var2)

parse code env (("add" : var1 : var2 : _) : xs) = go code env (+) var1 var2 xs
parse code env (("mul" : var1 : var2 : _) : xs) = go code env (*) var1 var2 xs
parse code env (("mod" : var1 : var2 : _) : xs) = go code env (mod) var1 var2 xs
parse code env (("div" : var1 : var2 : _) : xs) = go code env (quot) var1 var2 xs
parse code env (("eql" : var1 : var2 : _) : xs) = go code env (\a b -> fromEnum $ a == b) var1 var2 xs
parse code env xss@(("inp" : var1) : xs) = do
  guard (parse' (M.map (\a -> (a, a)) env) xss)
  n <- code
  ns <- parse code (putEnv env "w" n) xs
  return (n : ns)
parse code env [] = do
  guard (getEnv env "z" == 0)
  return ([])

q :: IO (Int, Int)
q = do
  xs <- (fmap words . lines) <$> readFile "./data21/day24.txt"
  let xmin = head $ parse [1 .. 9] (M.fromList $ [("x", 0), ("y", 0), ("z", 0)]) xs
  let xmax = head $ parse (reverse [1 .. 9]) (M.fromList $ [("x", 0), ("y", 0), ("z", 0)]) xs

  return (read $ concatMap show xmin, read $ concatMap show xmax)

-- >>> q
