module Y21.Day18 (q) where

import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Debug.Trace (trace)

-- zipper definition is from https://wiki.haskell.org/Zipper

data Tree a = Bin (Tree a) (Tree a) | Leaf a deriving (Eq)

data Cxt a = Top | L (Cxt a) (Tree a) | R (Tree a) (Cxt a)
  deriving (Show)

instance (Show a) => Show (Tree a) where
  show (Leaf x) = show x
  show (Bin l r) = "[" ++ show l ++ "," ++ show r ++ "]"

type Loc a = (Tree a, Cxt a)

make :: Tree a -> Loc a
make t = (t, Top)

moveLeft :: (Show a) => Loc a -> Loc a
moveLeft (Bin l r, c) = (l, L c r)
moveLeft x = error (show x)

moveRight :: Loc a -> Loc a
moveRight (Bin l r, c) = (r, R l c)

moveUp :: Loc a -> Loc a
moveUp (t, L c r) = (Bin t r, c)
moveUp (t, R l c) = (Bin l t, c)

upmost :: Loc a -> Loc a
upmost l@(t, Top) = l
upmost l = upmost (moveUp l)

modify :: Loc a -> (Tree a -> Tree a) -> Loc a
modify (t, c) f = (f t, c)

--- definition from https://wiki.haskell.org/Zipper ends
---

leftNode :: (Show a) => Loc a -> Maybe (Loc a)
leftNode (_, Top) = Nothing
leftNode x@(_, L {}) = leftNode $ moveUp x
leftNode x@(_, R {}) = Just $ rightmostNode (moveLeft $ moveUp x)
  where
    rightmostNode x@(Leaf {}, _) = x
    rightmostNode x = rightmostNode $ moveRight x

rightNode :: (Show a) => Loc a -> Maybe (Loc a)
rightNode (_, Top) = Nothing
rightNode x@(_, L {}) = Just $ leftmostNode (moveRight $ moveUp x)
  where
    leftmostNode x@(Leaf {}, _) = x
    leftmostNode x = leftmostNode $ moveLeft x
rightNode x@(_, R {}) = rightNode $ moveUp x

findNode = go 4
  where
    go _ (Leaf {}, _) = Nothing
    go n x@(Bin Leaf {} Leaf {}, _) | n <= 0 = Just x
    go n x = go (n - 1) (moveLeft x) <|> go (n - 1) (moveRight x)

findSplit x@(Leaf a, _) = if a >= 10 then Just x else Nothing
findSplit x = findSplit (moveLeft x) <|> findSplit (moveRight x)

splitOne tree = do
  (fst . upmost . go) <$> (findSplit $ make tree)
  where
    go (Leaf a, c) = (Bin (Leaf (a `div` 2)) (Leaf (a - (a `div` 2))), c)
    go x = x

magnitude (Bin a b) = 3 * magnitude a + 2 * magnitude b
magnitude (Leaf a) = a

explode tree = case findNode $ make tree of
  Nothing -> Nothing
  Just x -> let x' = fst $ upmost $ go x in (explode $ x') <|> Just x'
  where
    go (Bin (Leaf a) (Leaf b), c) =
      let l1 = (Leaf 0, c)
          l2 = fromMaybe l1 $ (leftNode l1 >>= (rightNode . process a))
       in fromMaybe l2 $ (rightNode l2 >>= (leftNode . process b))
    process n rn = modify rn (\(Leaf a) -> Leaf (a + n))

process x = explode x <|> splitOne x

iterateMaybe f x = x : go x
  where
    go x = case f x of
      Nothing -> []
      Just y -> y : go y

add l r = last $ iterateMaybe process (Bin l r)

q = do
  xs@(a : b : _) <- (fmap (fst . parseLine) . lines) <$> readFile "./data21/day18.txt"
  let res = magnitude $ foldl1 add xs
  let res2 = maximum $ concat $ [[magnitude $ add x y, magnitude $ add y x] | (i, x) <- zip [0 ..] xs, (j, y) <- zip [0 ..] xs, i /= j]

  return (res, res2)

parseLine :: [Char] -> (Tree Int, [Char])
parseLine ('[' : xs) =
  let (l, (',' : xs')) = parseLine xs
      (r, xs'') = parseLine xs'
   in case xs'' of
        ']' : xs'' -> (Bin l r, xs'')
        _ -> (Bin l r, xs'')
parseLine (a : xs) | a <= '9' && a >= '0' = (Leaf (read [a]), xs)
parseLine x = error x

-- >>> q

