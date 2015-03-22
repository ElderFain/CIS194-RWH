-- Validate a Credit Card # is legit

toDigits :: Integer -> [Integer]
toDigits x | x <=0 = []
toDigits x = toDigits (x `div` 10) ++ [x `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev x | x <=0 = []
toDigitsRev x = x `mod` 10 : toDigitsRev (x `div` 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x:y:zs) = x * 2: y  : doubleEveryOther zs

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:[]) = x
sumDigits (x:y) = sumDigits (toDigits x) + sumDigits y

validate :: Integer -> Bool
validate x = ( sumDigits (doubleEveryOther (toDigits x) ) ) `mod` 10 == 0

-- Hanoi Peg Mover
type Peg = String
type Move = (Peg, Peg)
-- hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]


data Thing = Shoe
			| Ship
			| SealingWax
			| Cabbage
			| King
	deriving Show

data FailableDouble = Failure
					| OK Double
	deriving Show

safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv x y = OK ( x / y )

failureToZero :: FailableDouble -> Double
failureToZero Failure = 0
failureToZero (OK d) = d


lastButOne xs = head $ (drop (length xs-2) xs)

-- Algebraic Data Types
data List a = Cons a (List a)
            | Nil
              deriving (Show)


fromList (x:xs) = Cons x (fromList xs)
fromList []     = Nil

-- Write the inverse of fromList; take `List a` and generate `[a]`
-- Cons 'd' (Cons 'u' (Cons 'r' (Cons 'i' (Cons 'a' (Cons 'n' Nil)))))
fromList' (Cons x xs) = x : fromList' xs
fromList' Nil = []


-- Define a tree type that has only one constructor, like our Java example.
-- Instead of the Empty constructor, use the Maybe type to refer to a node's children

-- class Example 
-- {
--     static Tree<String> simpleTree()
--     {
-- 	return new Tree<String>(
--             "parent",
-- 	    new Tree<String>("left leaf", null, null),
-- 	    new Tree<String>("right leaf", null, null));
--     }
-- }


data Tree a = Node a (Tree a) (Tree a)
			| Maybe


