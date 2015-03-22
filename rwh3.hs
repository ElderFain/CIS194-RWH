import Data.List
import Data.Ord
import Data.Function

data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

-- Real World Haskell Chapter 3
-- Write a function that computes the number of elements in a list. 
-- To test it, ensure that it gives the same answers as the standard length function.
length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + (length' xs)

mean' :: Fractional a => [a] -> a
mean' [] = 0
mean' xs = sum' xs / fromIntegral (length' xs)
  where sum' [] = 0
        sum' (y:ys) = y + (sum' ys)

palindrome :: [a] -> [a]
palindrome [] = []
palindrome (x:xs) = x : palindrome xs ++ [x]

isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome x = x == reverse x

sortByLength :: [[a]] -> [[a]]
sortByLength x = sortBy (compare `on` length) x

intersperse' :: a -> [[a]] -> [a]
intersperse' x (y:[]) = y
intersperse' x (y:ys) = y ++ [x] ++ (intersperse' x ys)

treeDepth :: Tree a -> Int
treeDepth Empty = 0
treeDepth (Node _ b c) = 1 + treeDepth b + treeDepth c

data Direction = Left
                | Right
                | Straight
                deriving (Show)
