module Lab where

evens :: [Integer] -> [Integer]
evens xs = [x | x <- xs, even x]

-- Exercise 0
-- sum . evens $ [827305 .. 927104]
-- 43772529500

-- Exercise 1
-- sum . evens $ []
-- 0

-- Exercise 2
-- sum . evens $ [1,3..] does not terminate


squares :: Integer -> [Integer]
squares n = [x * x | x <- [1..n]]

sumSquares :: Integer -> Integer
sumSquares n = sum (squares n)

-- Exercise 3
-- Integer -> [Integer]

-- Exercise 4
-- sumSquares 50
-- 42925

squares' :: Integer -> Integer -> [Integer]
squares' m n = [x * x | x <- [(n + 1)..(m + n)]]

sumSquares' :: Integer -> Integer
sumSquares' x = sum . uncurry squares' $ (x, x)

{- Modify the previous definition of squares such that it now takes two non-bottom Integer arguments, m >= 0 and n >= 0
   and returns a list of the m square numbers that come after the first n square numbers.

   Example:
   squares' 4 2 = [3*3, 4*4, 5*5, 6*6]
   squares' 2 0 = [1*1, 2*2]
   squares' 0 2 = []
   squares' 0 0 = []
   We can define a new sumSquares' function as follows:

   sumSquares' x = sum . uncurry squares' $ (x, x) -}

-- Exercise 5
-- What is the value of: sumSquares' 50?
-- 295425

-- Exercise 6
-- Using the squares' function that you've defined in the previous exercise, what is the value of: sum $ squares' 10 0:
-- 385

-- Exercise 7
-- Again using the squares' function that you've defined previously, what is the value of: sum $ squares' 0 10:
-- 0


-- Exercise 8

{- Using a list comprehension, define a function coords :: Integer -> Integer -> [(Integer, Integer)] that returns a list of all coordinate pairs on an [0..m] × [0..n] rectangular grid, where m and n are non-bottom Integers >= 0.

   Example:

   coords 1 1 = [(0,0), (0,1), (1,0), (1,1)]
   coords 1 2 = [(0,0), (0,1), (0,2), (1,0), (1, 1), (1, 2)] -}

coords :: Integer -> Integer -> [(Integer,Integer)]
coords m n = [(x, y) | x <- [0..m], y <- [0..n]]

-- What is the value of: foldr (-) 0 . map (uncurry (*)) $ coords 5 7
-- - 60

-- Exercise 9
-- The expression a f b g c is equivalent to:
-- ((a f) b) g c

-- Exercise 10
-- The type a -> f -> b -> g -> c is equivalent to:
-- a -> (f -> b -> (g -> c))

-- Exercise 11
-- The type (a, f, b, g, c) is equivalent to:
-- ((a, f, b, g, c))

-- Exercise 12
-- The expression (a, f, b, g, c) is equivalent to:
-- ((a, f, b, g, c))

