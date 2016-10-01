module Homework where

-- Exercise 0:
-- Parenthesise the following arithmetic expressions: --
2 ^ 3 * 4
2 * 3 + 4 * 5
2 + 3 * 4 ^ 5


(2 ^ 3) * 4
(2 * 3) + (4 * 5)
2 + (3 * (4 ^ 5))


-- Exercise 1:
{- The script below contains syntactic errors. Correct these errors and then choose right answer.
   N = a `div` length xs
     where	a = 10
           xs = [1,2,3,4,5]
-}

n = a `div` length xs
  where a = 10
        xs = [1, 2, 3, 4, 5]

-- Exercise 2:
{- The library function last, which selects the last element of a non-empty list, can be defined in terms of the library
   functions introduced in this chapter. Choose all possible definitions. -}

myLast xs = head (drop (length xs - 1) xs)

myLast' xs = xs !! (length xs - 1)

myLast'' xs = head (reverse xs)


-- Exercise 3:
{- The library function init, which removes the last element from a non-empty list, can be defined in terms of the library
   functions introduced in this chapter. Choose all possible definitions. -}

myInit xs = reverse (tail (reverse xs))


-- Exercise 4:
{-
Given the following examples, give another possible calculation for the result of double (double 2)

double (double 2)
=   { applying the inner double }
double (2 + 2)
=   { applying + }
double 4
=   { applying double }
4 + 4
=   { applying + }
8


double (double 2)
=   { applying the outer double }
(double 2) + (double 2)
=   { applying the first double }
(2 + 2) + (double 2)
=   { applying the first + }
4 + (double 2)
=   { applying double }
4 + (2 + 2)
=   { applying the second + }
4 + 4
=   { applying + }
8


-}

{-
double (double 2)
= { applying the inner double }
double (2 + 2)
= { applying double }
(2 + 2) + (2 + 2)
= { applying the ﬁrst + }
4 + (2 + 2)
= { applying the second + }
4+4
= { applying + }
8
-}


-- Exercise 5:
{-
  Show that sum [x] = x for any number x.
  Sum is defined as follows:
-}

sum [] = 0
sum (x:xs) = x + sum xs

{-
sum [x]
=   { applying sum }
x + sum []
=   { applying sum }
x + 0
=   { applying + }
x
-}

-- Exercise 6:

{-
Define a function product that produces the product of a list of numbers, and show using your definition that
product [2, 3, 4] = 24.
-}

product [] = 1
product (x:xs) = x * product xs

-- Exercise 7:
{-
How should the definition of the function qsort be modified so that it produces a reverse sorted version of a list?
Choose all possible definitions.
-}


myQsort [] = []
myQsort (x:xs) = myQsort larger ++ [x] ++ myQsort smaller
  where smaller = [a | a <- xs, a <= x]
        larger = [b | b <- xs, b > x]

myQsort' [] = []
myQsort' (x:xs) = myQsort' larger ++ [x] ++ myQsort' smaller
  where smaller = [a | a <- xs, a < x || a == x]
        larger = [b | b <- xs, b > x]

myQsort''' [] = []
myQsort''' (x : xs)
  = reverse
      (reverse (myQsort''' smaller) ++ [x] ++ reverse (myQsort''' larger))
  where smaller = [a | a <- xs, a <= x]
        larger = [b | b <- xs, b > x]

-- Exercise 8:
-- What would be the effect of replacing <= by < in the definition of qsort? Choose all possible effects.

-- Duplicate elements are removed from the sorted list.
-- The sorting function will only work correctly on some inputs.