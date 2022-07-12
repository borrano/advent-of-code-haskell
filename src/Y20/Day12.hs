module Y20.Day12 (q) where

import Util (readLinesString)

data Dir = N | E | S | W deriving (Enum, Bounded, Show)

add :: forall t. (Enum t, Bounded t) => t -> Int -> t
add d n = toEnum $ (n' + n) `mod` bound'
  where
    n' = fromEnum d
    bound' = (fromEnum (maxBound :: t)) + 1

data Action = Forward Int | Rotate Int | Move Dir Int deriving (Show)

move N i (a, b) = (a + i, b)
move S i (a, b) = (a - i, b)
move W i (a, b) = (a, b - i)
move E i (a, b) = (a, b + i)

action (dir, pos) (Forward i) = (dir, move dir i pos)
action (dir, pos) (Move d i) = (dir, move d i pos)
action (dir, pos) (Rotate i) = (add dir i, pos)

parseLine ('R' : xs) = Rotate (read xs `div` 90)
parseLine ('L' : xs) = Rotate ((4 + read xs `div` (-90)) `mod` 4)
parseLine ('N' : xs) = Move N (read xs)
parseLine ('E' : xs) = Move E (read xs)
parseLine ('S' : xs) = Move S (read xs)
parseLine ('W' : xs) = Move W (read xs)
parseLine ('F' : xs) = Forward (read xs)

manhattan (a, b) = abs a + abs b

action2 ((x, y), (a, b)) (Forward i) = ((x + i * a, y + i * b), (a, b))
action2 (pos, waypoint) (Move d i) = (pos, move d i waypoint)
action2 (pos, waypoint) (Rotate 0) = (pos, waypoint) -- (add dir i, pos)
action2 (pos, waypoint@(a, b)) (Rotate n) = action2 (pos, (-b, a)) (Rotate (n - 1)) -- (add dir i, pos)

calculate2 = manhattan . fst . foldl action2 ((0, 0), (1, 10))

calculate = manhattan . snd . foldl action (E, (0, 0))

q = do
  xs <- readLinesString "./data20/day12.txt"
  let parsed = fmap parseLine xs
  return (calculate parsed, calculate2 parsed)