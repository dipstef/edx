module Homework where

import Prelude hiding ((^), (!!))

-- Exercise 0 --
-- Choose all correct definitions of the exponentiation operator ^ for non-negative integers (including 0).

(^) :: Int -> Int -> Int
m ^ 0 = 1
m ^ n = m * m ^ (n - 1)

(^) :: Int -> Int -> Int
m ^ 0 = 1
m ^ n = m * (^) m (n - 1)


-- Exercise 1 --

-- Using the following definition, show how length [1, 2, 3] is evaluated.

myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + length xs

{-
length [1,2,3]
= { applying length }
1 + length [2,3]
= { applying length }
1 + (1 + length [3])
= { applying length }
1 + (1 + (1 + length []))
= { applying length }
1 + (1 + (1 + 0))
= { applying + }
1 + (1 + 1)
= { applying + }
1 + 2
= { applying + }
3
-}

-- Exercise 2 --
-- Using the following definition, show how drop 3 [1, 2, 3, 4, 5] is evaluated.

myDrop :: Int -> [a] -> [a]
myDrop 0 xs = xs
myDrop n [] = []
myDrop n (_:xs) = drop (n - 1) xs


{-
drop 3 [1,2,3,4,5]
= { applying drop }
drop 2 [2,3,4,5]
= { applying drop }
drop 1 [3,4,5]
= { applying drop }
drop 0 [4,5]
= { applying drop }
[4,5]
-}


-- Exercise 3 --
-- Using the following definition, show how init [1, 2, 3] is evaluated.

myInit :: [a] -> [a]
myInit [_] = []
myInit (x:xs) = x : init xs

{-
init [1,2,3]
= { applying init }
1 : init [2,3]
= { applying init }
1 : 2 : init [3]
= { applying init }
1 : 2 : []
= { list notation }
[1,2]
-}

-- Exercise 4 --
-- Choose all correct definitions for the function that decides if all logical values in a list are True:

myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) = x && myAnd xs

myAnd2 :: [Bool] -> Bool
myAnd2 [] = True
myAnd2 (x:xs)
 | x = myAnd2 xs
 | otherwise = False

myAnd3 :: [Bool] -> Bool
myAnd3 [] = True
myAnd3 (x:xs)
 | x == False = False
 | otherwise = myAnd3 bs

myAnd4 :: [Bool] -> Bool
myAnd4 [] = True
myAnd4 (x:xs) = and xs && x

-- Exercise 5 --
-- Choose the correct definition for the function that concatenates a list of lists:

myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (x:xs) = x ++ concat xs

-- Exercise 6 --
-- Choose the correct definition for the function that produces a list with n identical elements:

myReplicate :: Int -> a -> [a]
myReplicate 0 _ = []
myReplicate n x = x : myReplicate (n - 1) x

-- Exercise 7 --
-- Choose the correct definition for the function that selects the n th element of a list. We start counting at 0.

(!!) :: [a] -> Int -> a
(x:_) !! 0 = x
(_:xs) !! n = xs !! (n - 1)

-- Exercise 8 --
-- Choose the correct definition for the function that decides if a value is an element of a list:

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem e (x:xs)
  | e == x = True
  | otherwise = myElem e xs

-- Exercise 9 --
-- Choose the correct definition for the function merge :: Ord a => [a] -> [a] -> [a] that merges two sorted lists in
-- ascending order to give a single sorted list in ascending order. For example:


myMerge :: Ord a => [a] -> [a] -> [a]
myMerge [] ys = ys
myMerge xs [] = xs
myMerge (x:xs) (y:ys)
  | x <= y = x : myMerge xs (y:ys)
  | otherwise = y : myMerge (x:xs) ys

-- Exercise 10 --
{-
Choose the correct definition for the function msort :: Ord a => [a] -> [a] that implements merge sort, in which the
empty list and singleton lists are already sorted, and any other list is sorted by merging together the two lists that
result from sorting the two halves of the list separately. The solutions can use the function merge from the previous
exercise and the function halve that splits a list into two halves whose lengths differ by at most one.
-}

halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = myMerge (msort ys) (msort zs)
  where (ys, zs) = halve xs
