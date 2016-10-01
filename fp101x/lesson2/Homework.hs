module Homework where

-- Exercise 0
-- What is the type of the following value: ['a', 'b', 'c']
-- [Char]

-- Exercise 1
-- What is the type of the following value: ('a', 'b', 'c')
-- (Char, Char, Char)


-- Exercise 2
-- What is the type of the following value: [(False , '0'), (True , '1')]
--[(Bool, Char)]

-- Exercise 3
-- What is the type of the following value: ([False, True], ['0', '1'])
-- ([Bool], [Char])

-- Exercise 4
-- What is the type of the following value: [tail, init, reverse]
-- [[a] -> [a]]


-- Exercise 5
-- What is the type of the following function:
seconds xs = head (tail xs)
-- [a] -> a

-- Exercise 6
-- What is the type of the following function:
swap (x, y) = (y, x)
-- (a, b) -> (b, a)

-- Exercise 7
-- What is the type of the following function:
pair (x, y) = (x, y)
-- a -> b -> (a, b)

-- Exercise 8
-- What is the type of the following function:
double x = x * 2
-- Num a => a -> a

-- Exercise 9
-- What is the type of the following function:
palindrome xs = reverse xs == xs
-- Eq a => [a] -> Bool

-- Exercise 9
-- What is the type of the following function:
twice f x = f (f x)
-- (a -> a) -> a -> a

-- Exercise 11
{- Is it feasible for function types (in general) to be instances of the Eq class?
   Hint: Two functions of the same type are equal iff they always return equal results for equal arguments. -}
-- Infeasible in general; only feasible for some functions.

-- Exercise 12
-- Which of the following is not a valid list in Haskell:
-- ['1',['2','3']]

-- Exercise 13
-- Which of the following is not a valid list in Haskell:
-- [1,[2,3],4]

-- Exercise 14
-- The expression ["False", "True"] has type:
-- [String]

-- Exercise 15
-- The expression ([False, True], False) has type:
-- ([Bool],Bool)

-- Exercise 16
-- The expression ("1,2","3,4") has type:
-- (String,String)

-- Exercise 17
-- The expression [(1,True),(0,False)] has type:
-- [(Int,Bool)]

-- Exercise 18
-- What is the type of f xs = take 3 (reverse xs)
-- [a] -> [a]

-- Exercise 19
-- a -> b -> c -> d
-- a -> (b -> (c -> d))

-- Exercise 20
-- Which of the following expressions contains a type error:
-- [1,2,3] ++ 4

