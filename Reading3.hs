--Exercise 3.1
x0 :: [Char]
x0 = ['a','b','c']
--Because it is a char list
x1 :: (Char,Char,Char)
x1 = ('a','b','c')
--Because it is a char tuple
x2 :: [(Bool,Char)]
x2 = [(False,'0'),(True,'1')]
--Because it is a list of pair of Boolean value
x3 :: ([Bool],[Char])
x3 = ([False,True],['0','1'])
--it is a pair of two list, the first list is a Boolean list and the second one is a Char list
x4 :: [[a] -> [a]]
x4 = [tail,init, reverse]
--since all functions including tail, init and reverse are the type of [a] -> [a], so a list of them is just add to square brackets
--Exercise 3.2
bools :: [Bool]
bools = [False,True]
--Write out a list of Boolean values
nums :: [[Int]]
nums = [[1,2],[3,4]]
--Write out a list of list of integers
add :: Int -> Int -> Int -> Int
add x y z = x + y + z
--Write a function with three integer inputs and the output is still a integer
copy :: a -> (a,a)
copy x = (x,x)
--Write a function to copy a single integer to a pair of integer
apply :: (a -> b) -> a -> b
apply f x = f x
--Write a function of type (a -> b) and apply it to a
--Exercise 3.3
second :: [a] -> a
second xs = head (tail xs)
--The function is to select the second index in a list so it converts a list to a single element
swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)
--The functino is to swap the order of a pair so the input and output are both a pair
pair :: a -> b -> (a,b)
pair x y  = (x,y)
--The functino is to make two inputs into a pair
double :: Num a => a ->  a
double x = x * 2
--The function is just double the input, but be cared about the type class because Num type class cannot be multiplied
palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs
--The function is to check if a list is a palindrome list so it needs a list input and output a Boolean value, but be cared about the type class because the input needs to belong with type class Eq
twice :: (a -> a) -> a -> a
twice f x = f (f x)
--The function is to use a function two times, since the input is only one so the output would be the same.
go :: Integer -> Integer -> Integer
go s i = s/4 + i
