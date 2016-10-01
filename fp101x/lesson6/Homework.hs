module Homework where

-- Exercise 0
-- Choose the equivalent of the following list comprehension [f x | x <- xs, p x] expressed using higher-order functions.

f0 :: (a -> Bool) -> (a -> b) -> [a] -> [b]
f0 p f xs = [f x | x <- xs, p x]

f0' :: (a -> Bool) -> (a -> b) -> [a] -> [b]
f0' p f xs = map f (filter p xs)


-- Exercise 1 --
{- Choose all options that implement the Prelude function

   all: (a -> Bool) -> [a] -> Bool
   taking into account only finite, non-partial input lists with non-bottom values and where the predicate p always
   returns either True, or False, but not bottom.
-}

myAllA :: (a -> Bool) -> [a] -> Bool
myAllA p xs = and (map p xs)

myAllB :: (a -> Bool) -> [a] -> Bool
myAllB p = and . map p

myAllC :: (a -> Bool) -> [a] -> Bool
myAllC p = not . any (not . p)

myAllD :: (a -> Bool) -> [a] -> Bool
myAllD p xs = foldl (&&) True (map p xs)

myAllE :: (a -> Bool) -> [a] -> Bool
myAllE p = foldr (&&) True . map p

-- Exercise 2 --
{- Choose all options that implement the Prelude function

  any: (a -> Bool) -> [a] -> Bool

  taking into account only finite, non-partial input lists with non-bottom values and where the predicate p always
  returns either True, or False, but not bottom. -}

myAnyA :: (a -> Bool) -> [a] -> Bool
myAnyA p = or . map p

myAnyB :: (a -> Bool) -> [a] -> Bool
myAnyB p xs = length (filter p xs) > 0

myAnyC :: (a -> Bool) -> [a] -> Bool
myAnyC p = not . null . dropWhile (not . p)

myAnyD :: (a -> Bool) -> [a] -> Bool
myAnyD p xs = not (all (\x -> not (p x)) xs)

myAnyE :: (a -> Bool) -> [a] -> Bool
myAnyE p xs = foldr (\x acc -> (p x) || acc) False xs

-- Exercise 3 --
{- Choose the option that implements the Prelude function

   takeWhile :: (a -> Bool) -> [a] -> [a]

   taking into account only finite, non-partial input lists with non-bottom values and where the predicate p always
   returns either True, or False, but not bottom. -}

myTakeWhileA :: (a -> Bool) -> [a] -> [a]
myTakeWhileA _ [] = []
myTakeWhileA p (x:xs)
  | p x = x : myTakeWhileA p xs
  | otherwise = []

myTakeWhileB :: (a -> Bool) -> [a] -> [a]
myTakeWhileB p = fst . foldl (\(xs, c) x -> if (p x) && c then (xs ++ [x], True)
                                            else (xs, False)) ([], True)

-- Exercise 4 --
{- Choose the option that implements the Prelude function

   dropWhile :: (a -> Bool) -> [a] -> [a]

   taking into account only finite, non-partial input lists with non-bottom values and where the predicate p always
   returns either True, or False, but not bottom.-}

myDropWhileA :: (a -> Bool) -> [a] -> [a]
myDropWhileA _ [] = []
myDropWhileA p (x:xs)
  | p x = myDropWhileA p xs
  | otherwise = (x:xs)

myDropWhileB :: (a -> Bool) -> [a] -> [a]
myDropWhileB p = fst . foldl (\(xs, c) x -> if (p x) && c then (xs, True)
                                            else (xs ++ [x], False)) ([], True)

-- Exercise 5 --

{- Choose the option that implements the Prelude function

   map :: (a -> b) -> [a] -> [b[

   taking into account only finite, non-partial input lists with non-bottom values and where the mapping function does
   not return bottom.-}

myMapA :: (a -> b) -> [a] -> [b]
myMapA f = foldr (\x ys -> (f x) : ys) []

myMapB :: (a -> b) -> [a] -> [b]
myMapB f = foldl (\ys x -> ys ++ [f x]) []

-- Exercise 6 --

{- Choose the option that implements the Prelude function

   filter :: (a->Bool) -> [a] -> [a]

   taking into account only finite, non-partial input lists with non-bottom values and where the predicate p always
   returns either True, or False, but not bottom. -}

myFilterA :: (a -> Bool) -> [a] -> [a]
myFilterA p = foldl (\xs x -> if p x then xs ++ [x] else xs) []

myFilterB :: (a -> Bool) -> [a] -> [a]
myFilterB p = foldr (\x xs -> if p x then x : xs else xs) []

-- Exercise 7 --

{- Choose a definition for the function dec2int :: [Integer] -> Integer that converts a finite, non-partial list of
   non-bottom Integer digits, that represents a decimal number, into the non-bottom Integer this list represents.

   For example:

   > dec2int [2, 3, 4, 5]
   2345
   > dec2int []
   0
   > dec2int [0, 0, 0, 0]
   0


   For additional understanding, try to experiment with infinite and partial lists and see if you can spot any differences
   in behaviour for the various implementations.
-}

dec2intA :: [Integer] -> Integer
dec2intA = fst . foldr (\x (acc, c) -> (acc + x * 10 ^ c, c + 1)) (0, 0)

dec2intB :: [Integer] -> Integer
dec2intB = foldl (\acc x -> x + 10 * acc) 0

-- Exercise 8 --

{- Choose an explanation for why the following definition of sumsqreven is invalid:

   sumsqreven = compose [sum, map (^ 2), filter even] -}

-- The following function doesn't type check because sum has type [Int] -> Int
-- while map (^ 2) and filter even both have type [Int] -> [Int]. In Haskell
-- every element in a list has to be of the same type.

-- sumsqreven = compose [sum, map (^ 2), filter even]

compose :: [a -> a] -> (a -> a)
compose  = foldr (.) id

-- Exercise 9 --

{- Choose the correct definition for the Prelude function curry :: ((a, b) -> c) -> a -> b -> c , that converts a function
   that takes its arguments as a pair into a function that takes its arguments one at a time.

   For this exercise assume that bottom does not exist. -}

myCurryA :: ((a, b) -> c) -> a -> b -> c
myCurryA f a b = f (a, b)

myCurryB :: ((a, b) -> c) -> a -> b -> c
myCurryB = \f -> (\a -> (\b -> f (a, b)))

myCurryC :: ((a, b) -> c) -> a -> b -> c
myCurryC f = \x y -> f (x, y)

-- Exercise 10 --

{- Choose the definition for the Prelude function uncurry :: (a -> b -> c) -> (a, b) -> c, that converts a function that
   takes its arguments one at a time into a function that takes its arguments as a pair.

   For this exercise assume that bottom does not exist. -}

myUncurry :: (a -> b -> c) -> ((a, b) -> c)
myUncurry f = \(a, b) -> f a b

-- Exercise 11 --

{- Consider the following higher-order function unfold :: (b -> Bool) -> (b -> a) -> (b -> b) -> b -> [a] that encapsulates
   a simple pattern of recursion for producing a list.-}

unfold :: (b -> Bool) -> (b -> a) -> (b -> b) -> b -> [a]
unfold p h t x
  | p x = []
  | otherwise = h x : unfold p h t (t x)

{- The function unfold p h t x produces the empty list if the predicate p x is True. Otherwise it produces a non-empty
   list by applying the function h x to give the head of the generated list, and the function t x to generate another
   seed that is recursively processed by unfold to produce the tail of the generated list.

   For example, the function int2bin, that converts a non-negative integer into a binary number, with the least significant
   bit first, can be defined as:

   For example: -}

type Bit = Int

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = [n `mod` 2] : int2bin(n `div` 2)

{- > int2bin 13
   [1, 0, 1, 1]
   > int2bin (-0) -- Yes, 0 can be negative!
   []
   This function can be rewritten more compactly using unfold as follows:
-}

int2binB = unfold ( == 0) (`mod` 2) ('div' 2)

{- Next consider the function chop8 :: [Bit] -> [[Bit]] that takes a list of bits and chops it into lists of at most
   eight bits (assuming the list is finite, non-partial, and does not contain bottom): -}
chop8A :: [Bit] -> [[Bit]]
chop8A [] = []
chop8A bits = take 8 bits : chop8A (drop 8 bits)

chop8B :: [Bit] -> [[Bit]]
chop8B = unfold null (take 8) (drop 8)

-- Exercise 12 --

{- Following the previous question, choose an implementation of map :: (a -> b) -> [a] -> [b] using unfold.

   taking into account only finite, non-partial input lists with non-bottom values, and where the mapping function does
   not return bottom. -}

myMapC :: (a -> b) -> [a] -> [b]
myMapC f = unfold null (f . head)  tail

-- Exercise 13 --

{-- Choose an implementation of the Prelude function iterate :: (a -> a) -> a -> [a] using unfold. -}

myIterate :: (a -> a) -> a -> [a]
myIterate f = unfold (const False) id f

-- Exercises 14 --

{- Assuming f, g and h are not bottom, the following equality holds for all f, g and h of the correct type: -}
-- f . (g . h) = (f . g) . h

-- Exercise 15 --
-- Which of the following properties about lists is false:
-- [x] : xs = [x, xs]

-- Exercise 16 --
-- Which of the following properties about map and filter is true for all f, g and p of the correct type:
-- filter p . filter p = filter p

-- Exercise 17 --
-- Which of the following is true for all non-bottom f, g and p of the correct type, and finite, non-partial input lists
-- xs that contain no bottom values:
-- reverse (map f xs) = map f (reverse xs)

-- Exercise 18 --
-- Which of the following equations is true for all finite, non-partial lists xs and ys, with non-bottom values:
-- reverse (xs ++ ys) = reverse ys ++ reverse xs

-- Exercise 19 --
-- Which of the following expressions produces a finite list:
-- take 10 [1..]

-- Exercise 20 --
-- Which of the following statements about the Prelude function sum :: Num a => [a] -> a is false
-- sum is a higher-order function


-- Exercise 21 --
-- Pick one of the wrong statements about the Prelude function map :: (a -> b) -> [a] -> [b] :
-- map is a function with two arguments
-- map is an overloaded function

-- Exercise 22 --
-- Which of the following statements about the Prelude function foldr :: (a -> b -> b) -> b -> [a] -> b is false:
-- foldr is an overloaded function

-- Exercise 23 --
-- Which of the following statements about various Prelude functions is true:
-- take is a polymorphic function

-- Exercise 24 --
-- Which equation defines a function f that is overloaded:
-- f x = x > 3

-- Exercise 25 --
-- Which of the following expressions is equal to [1, 2, 3, 4]:
-- take 4 (iterate (+1) 1)

-- Exercise 26 --
-- Evaluating takeWhile even [2, 4, 5, 6, 7, 8] gives:
-- [2,4]

-- Exercise 27 --
-- Evaluating zip [1, 2] ['a', 'b', 'c'] gives:
-- [(1,'a'),(2,'b')]


-- Exercise 28 --
-- Evaluating foldr (-) 0 [1, 2, 3, 4] gives:
-- −2

-- Exercise 29 --
-- Evaluating filter even (map (+1) [1..5]) gives (Note: you can copy and paste this expression directly from edX intro Hugs!):
-- [2,4,6]

-- Exercise 30 --
-- Which of the following expressions is equal to filter p (map f xs), for all finite, non-partial lists xs with no
-- bottom values, and for all non-bottom f and p of the correct type:

-- [f x|x <- xs, p (f x)]

-- Exercise 31 --

{- After watching the jam session about Church Numerals, what could be a possible implementation for exponentiation?
   (Note: you have very many attempts to get this question correct)  -}


cExp :: CNat -> CNat -> CNat
cExp (CNat a) (CNat b) = CNat (b a)
