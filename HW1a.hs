module HW1a where

{- For all functions, please write the function's type above it
so that the compiler checks your inteded type with the function's
actual type.

Feel free to call helper functions or other problems' functions.
Unless stated otherwise in a particular problem, you may use any combination of language features we've covered for dealing with a variable input size. These features are higher order functions from the standard prelude, recursion,
or list comprehensions.
-}

{- 1. define a function allIncreasing, that when given a start number and a length
evaluates to a list of all lists of increasing integers
   up to the length, starting at the start number. For example

allIncreasing 10 3 = [[10], [10,11], [10,11,12]]

The list must be in exactly the above order of increasing length
-}
allIncreasing :: Int -> Int -> [[Int]]
allIncreasing x 0 = []
allIncreasing x 1 = [[x]]
allIncreasing x y = allIncreasing x (y-1) ++ [[x..(x+y-1)]]





{- 2. define a function allIncreasingEven, that when given a start number and a length
evaluates to a list of all lists of increasing even integers up to the length,
starting at the start number.

For example

allIncreasingEven 10 3 = [[10], [10,12], [10,12,14]]

If an odd number is given as the start, then start from the number below it. For example

allIncreasingEven 11 2 = [[10], [10,12]]

The list must be in exactly the above order of increasing length
-}
allIncreasingEven :: Int -> Int -> [[Int]]
allIncreasingEven x 0 = []
allIncreasingEven x 1 = if (x `mod` 2 == 0) then [[x]] else [[x-1]]
allIncreasingEven x y = allIncreasingEven x (y-1) ++ [x'] where x' = [x | x <- [(x-1)..(x+2*y-2)], x `mod` 2 == 0]


{- 3. define a function allIncreasingSubsequences, that when given integers s and e
evaluates to the list of all lists of increasing subsequences between s and e, inclusive.
For example

allIncreasingSubsequences 4 6 = [[4],[4,5],[4,5,6],[4,6],[5,6],[5],[6]]

You can return these in *any order*. No duplicates allowed, e.g., two [5]'s or two [4,5]'s in above example
	-}
createList :: Int -> Int -> [Int]
createList x y = [x..y]
subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map(x:) yss where yss = subs xs
allIncreasingSubsequences :: Int -> Int -> [[Int]]
allIncreasingSubsequences x y = tail(subs(createList x y))
{-
4. define a function sumsOrProducts that takes a list of 3-tuples of integers. The function should evaluate
to a list of all the third elements of the tuples, where the first and second elements of that tuple
sum to or multiply to the third element. For example
sumsOrProducts [(1,2,3), (4,5,20), (1,2,4), (10,11,30)] = [3, 20]

above, the first tuple has 1+2=3, so keep the third element 3. The second tuple has 4*5=20 so keep
the third element 20. The next two tuples do not produce results because their first two elements
do not sum or multiply to the third.

YOU MUST DEFINE sumsOrProducts using list comprehensions
-}
sumsOrProducts ::  [(Int,Int,Int)] -> [Int]
sumsOrProducts ns = [z | (x,y,z) <- ns, x * y == z || x + y == z]


{- 5. define a function sumsOrProducts', which does the same as above

   YOU MUST DEFINE sumsOrProducts using higher order functions from the standard prelude
-}
sumsOrProducts' :: [(Int,Int,Int)] -> [Int]
getThird :: (Int,Int,Int) -> Int
getThird (x,y,z) = z
correctFilter :: (Int,Int,Int) -> Bool
correctFilter (x,y,z) = x + y == z || x * y == z
sumsOrProducts' ns = map (getThird) (filter correctFilter ns)

{- 6. define a function sumsOrProducts'', which does the same as above

    YOU MUST DEFINE sumsOrProducts'' using recursion
-}
sumsOrProducts'' :: [(Int,Int,Int)] -> [Int]
sumsOrProducts'' [] = []
sumsOrProducts'' ((x,y,z):xs) = if ((x + y == z) || (x * y == z)) then ([z] ++ sumsOrProducts'' xs) else sumsOrProducts'' xs

{-
7. Define a higher order function tripleCollapse, which is a generalization of sumsOrProducts. The two generalizations are:
a. The 3-tuples ("triples") and the elements in the result list can have any type, so make
your function polymorphic

b. The function should take two functions for the predicate and the transformation

That is, it takes a
function p from (a,b,c) to Bool, a function g from (a, b, c) to d. 

For example

tripleCollapse (\(x,y,z) -> x > y) (\(x,y,z) -> x - y) [(11,14,16), (3,2,1), (4,0,10)] = [1, 4]

There are no requirements on which language features you use (list comprehension, recursion, or
other higher order functions).
-}
--7a
tripleCollapse :: ((a,b,c) -> Bool) -> ((a,b,c) -> d) -> [(a,b,c)] -> [d]
tripleCollapse f p ns =[p x | x <- ns, f x]