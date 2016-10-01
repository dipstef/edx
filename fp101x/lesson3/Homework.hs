module Homework where

-- Exercise 0
{-
Which of the following implementations correctly define a function halve :: [a] -> ([a], [a]) that splits an even-lengthed
list into two halves? Choose all correct implementations!
-}


-- Doesn't compile
-- halveA :: [a] -> ([a], [a])
-- halveA xs = (take n xs, drop n xs)
--  where n = length xs / 2

halveB :: [a] -> ([a], [a])
halveB xs = splitAt (length xs `div` 2) xs

halveC :: [a] -> ([a], [a])
halveC xs = (take (n `div` 2) xs, drop (n `div` 2) xs)
  where n = length xs

-- Doesn't compile
-- halveD :: [a] -> ([a], [a])
-- halveD xs = splitAt (length xs `div` 2)

-- Yields incorrect output
-- halveE :: [a] -> ([a], [a])
-- halveE xs = (take n xs, drop (n + 1) xs)
-- where n = length xs `div` 2

halveF :: [a] -> ([a], [a])
halveF xs = splitAt (div (length xs) 2) xs

-- Doesn't compile
-- halveG :: [a] -> ([a], [a])
-- halveG xs = splitAt (length xs / 2) xs

halveH :: [a] -> ([a], [a])
halveH xs = (take n xs, drop n xs)
  where n = length xs `div` 2


-- Exercise 1

{-
Which of the following implementations are valid for a function safetail :: [a] -> [a] that behaves as the library
function tail , except that safetail maps the empty list to itself, whereas tail produces an error in this case.
Choose all correct implementations!
-}

safetailA :: [a] -> [a]
safetailA xs = if null xs then [] else tail xs

safetailB :: [a] -> [a]
safetailB [] = []
safetailB (_ : xs) = xs

-- Doesn't work; non-exhaustive pattern match
-- safetailC :: [a] -> [a]
-- safetailC (_ : xs)
--  | null xs = [] -- this will never be true
--  | otherwise = tail xs

safetailD :: [a] -> [a]
safetailD xs
  | null xs = []
  | otherwise = tail xs

safetailE :: [a] -> [a]
safetailE xs
  | length xs == 0 = []
  | otherwise = tail xs

-- Doesn't work since the first pattern always matches
-- safetailF :: [a] -> [a]
-- safetailF xs = tail xs
-- safetailF [] = [] -- this will never match

safetailG :: [a] -> [a]
safetailG [] = []
safetailG xs = tail xs

-- Doesn't work; non-exhaustive pattern match
-- safetailH :: [a] -> [a]
-- safetailH [x] = [x]
-- safetailH (_ : xs) = xs


-- Exercise 2

-- Which of the following definitions is correct for the logical disjunction operator || (i.e. OR)? Choose all correct
-- implementations!

False `orA` False = False
_ `orA` _ = True

False `orB` b = b
True `orB` _ = True

-- This is the definition for xnor not or
-- b `orC` c
--  | b == c = True
--  | otherwise = False

b `orD` c
  | b == c = b
  | otherwise = True

b `orE` False = b
_ `orE` True = True

b `orF` c
  | b == c = c
  | otherwise = True

-- Incorrect logic and non-exhaustive pattern matching
-- b `orG` True = b
-- _ `orG` True = True

False `orH` False = False
False `orH` True = True
True `orH` False = True
True `orH` True = True

-- Exercise 3

{-
Which of the following definitions is correct for the logical conjunction operator && (i.e. AND)? Choose all
implementations that give the correct answer based on the TRUTH TABLE, thus not taking into account short-circuiting
-}

True `andA` True = True
_ `andA` _ = False

a `andB` b = if a then if b then True else False else False

-- Incorrect output on False `andC` False
-- a `andC` b = if not (a) then not (b) else True

-- Doesn't compile
-- a `andD` b = if a then b

-- Incorrect output on True `andE` True
-- a `andE` b = if a then if b then False else True else False

a `andF` b = if a then b else False

a `andG` b = if b then a else False

-- Exercise 4
-- Show how the curried function definition mult x y z = x * y * z can be understood in terms of lambda expressions.


mult :: Num a => a -> a -> a -> a
mult x y z = x * y * z

multLambda :: Num a => a -> (a -> (a -> a))
multLambda = \x -> (\y -> (\z -> x * y * z))

mult2 :: Num a => a -> (a -> a)
mult2 x = \y -> x * y

-- Exercise 5
-- The expression f x g y means:
-- ((f x) g) y

-- Exercise 6
-- The type signature f :: (a -> a) -> a indicates that the function f:

-- Exercise 7
-- Choose the correct implementation for the function remove :: Int -> [a] -> [a] which takes a number n and a list
-- and removes the element at position n from the list.
remove n xs = take n xs ++ drop (n+1) xs

-- Exercise 8
-- What is the output of the function call funct 3 [1, 2, 3, 4, 5, 6, 7]? The function funct is defined as:

funct :: Int -> [a] -> [a]
funct x xs = take (x + 1) xs ++ drop x xs

-- [1, 2, 3, 4, 4, 5, 6, 7]