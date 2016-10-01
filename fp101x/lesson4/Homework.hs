module Homework where

-- Exercise 0
-- Which of these expressions calculates the sum: 1^2+2^2+...+100^2 of the first one hundred integer squares?

sum100 = sum [x ^ 2 | x <- [1..100]]


-- Exercise 1
{- The library function replicate :: Int -> a -> [a] produces a list of identical elements.
   Choose one possible implementation for this function. For example:

> replicate 3 True
  [True, True, True] -}

myReplicate :: Int -> a -> [a]
myReplicate n a = [a | _ <- [1..n]]

-- Exercise 2
{- A triple (x, y, z) of positive integers is pythagorean if x^2+y^2=z^2.
   Choose the correct implementation for the function

   pyths :: Int -> [(Int, Int, Int)] that returns the list of all pythagorean triples whose components are at most a given limit.

   > pyths 10
   [(3, 4, 5), (4, 3, 5), (6, 8, 10), (8, 6, 10)]
-}

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n],
                       (x ^ 2 + y ^ 2) == (z ^ 2)]

-- Exercise 3

{- A positive integer is perfect if it equals the sum of its factors, excluding the number itself.
   Choose the correct definition of the function perfects :: Int -> [Int] that returns the list of all perfect numbers
   up to a given limit.

   Note: factors is not a library function but is defined in the lecture.

   factors :: Int -> [Int]
   factors n = [x | x <- [1..n], n `mod` x == 0]


   > perfects 500
   [6, 28, 496]

-}


perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], isPerfect x]
  where isPerfect num = sum (init (factors num)) == num

-- Exercise 4
-- The following list comprehension:

pairs1 :: [(Int, Int)]
pairs1 = [(x,y) | x <- (1,2,3), y <- (4,5,6)]

{- can be re-expressed using two or more comprehensions with single generators.
   Choose the implementation that is equivalent to the one above.-}

pairs2 :: [(Int, Int)]
pairs2 = concat [[(x, y) | y <- [4, 5, 6]] | x <- [1, 2, 3]]

-- Exercise 5
-- Redefine the function positions discussed in the lecture, using the function find:

find :: (Eq a) => a -> [(a, b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

positions :: (Eq a) => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..n])
  where n = length xs - 1


-- Exercise 6 --
{-
The scalar product of two lists of integers xs and ys of length n is given by the sum of the products of corresponding integers:

sum ( (xs !! i) * (ys !! i) ) for i = 0 to n-1
Choose the correct definition of

scalarproduct :: [ Int ] -> [ Int ] -> Int that returns the scalar product of two lists.

> scalarproduct [1, 2, 3] [4, 5, 6]
32

-}

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x, y) <- xs `zip` ys]

-- Exercise 7 --

import Data.Char

let2int :: Char -> Int
let2int c = ord c - ord x

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c
    | isLower c = int2let ((let2int c + n) `mod` 26)
    | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]


{- Modify the Caesar cipher program to also handle upper-case letters. Given the following string

   "Think like a Fundamentalist Code like a Hacker", encode it with your modified program (using shift size 13)
   and choose the correct outputModify the Caesar cipher program to also handle upper-case letters.
-}


let2int :: Char -> Int
let2int c = let x = if isLower c then 'a' else 'A' in ord c - ord x

int2let :: Int -> Bool -> Char
int2let n isLower = let x = if isLower then 'a' else 'A' in chr (ord x + n)

shift :: Int -> Char -> Char
shift _ ' ' = ' '
shift n c = int2let ((let2int c + n) `mod` 26) (isLower c)

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

-- encode 13 "Think like a Fundamentalist Code like a Hacker"
-- "GuvaxGyvxrGnGShaqnzragnyvfgGPbqrGyvxrGnGUnpxre"


-- Exercise 8 --
-- Evaluating [(x, y) | x <- [1, 2], y <- [1, 2]] gives:

ex8 :: [(Int, Int)]
ex8 = [(x, y) | x <- [1, 2], y <- [1, 2]]

[(1,1),(1,2),(2,1),(2,2)]

-- Exercise 9 --
-- Evaluating [x | x <- [1, 2, 3], y <- [1..x]] gives:

ex9 :: [Int]
ex9 = [x | x <- [1, 2, 3], y <- [1..x]]

[1,2,2,3,3,3]

-- Exercise 10 --
-- Evaluating sum [x | x <- [1..10], even x] gives:

ex10 :: Int
ex10 = sum [x | x <- [1..10], even x]

-- 30

-- Exercise 11 --
-- The equation xs = 1 : [x + 1 | x <- xs] defines:

ex11 :: [Int]
ex11 = 1 : [x + 1 | x <- ex11]

-- xs = [1,2,3,...]

-- Exercise 12 --
{- Choose the correct definition of the function riffle :: [a] -> [a] -> [a] that takes two lists of the same length
   and interleaves their elements in turn about order.

   For example:
   riffle [1,2,3] [4,5,6] = [1, 4, 2, 5, 3, 6]
   -}

riffle :: [a] -> [a] -> [a]
riffle xs ys = concat [[x, y] | (x, y) <- xs `zip` ys]

-- Exercise 13 --

{-
Choose the correct definition for the function divisors :: Int -> [Int] that returns the divisors of a natural number.

For example:

divisors 15 = [1, 3, 5, 15]

The function divides :: Int -> Int -> Bool decides if one integer is divisible by another.
(Note: You need to implement this function yourself.)

For example:

divides 15 2 = False
divides 15 3 = True
-}


-- This function is kind of awkward since when used in the infix form
-- it reads dividend `divides` divisor, but that's how it was defined
-- in the exercise.
divides :: Int -> Int -> Bool
divides x y = x `mod` y == 0

divisors :: Int -> [Int]
divisors x = [d | d <- [1..x], x `divides` d]
