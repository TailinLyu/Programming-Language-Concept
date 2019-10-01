module TailHeadLists where

{- fill in the two constructors Nil and Cons for this data type -}
{-
data ThList a = ??
-}
data ThList a = Nil | Cons (ThList a) a
{- thappend is like ++ -}
thappend :: (ThList a) -> (ThList a) -> (ThList a)
thappend Nil Nil = Nil
thappend Nil (Cons xs x) = Cons xs x
thappend (Cons xs x) ys = Cons (thappend xs ys) x
{- thlength is like length, but our type is specific to lists not "Foldables" -}
thlength :: ThList a -> Int
thlength Nil = 0
thlength (Cons xs _) = 1 + thlength(xs) 

{- thmap is like map -}
thmap :: (a -> b) -> (ThList a) -> (ThList b)
thmap f Nil = Nil
thmap f (Cons xs x) = Cons (thmap f xs) (f x)
{- thfilter is like filter -}

thfilter :: (a -> Bool) -> (ThList a) -> (ThList a)
thfilter p Nil = Nil
thfilter p (Cons xs x) | p x = Cons (thfilter p xs) x
 | otherwise = thfilter p xs
{- thfoldl is like foldl, but our type is specific to lists not "Foldables" -}
thfoldl :: (b -> a -> b) -> b -> ThList a -> b
thfoldl f v Nil = v
thfoldl f v (Cons xs x) = thfoldl f (f v x) xs

value :: ThList a -> [a]
value Nil = []
value (Cons Nil x) = [x]
value (Cons xs x) = value (Cons Nil x) ++ value xs
{- thintersperse is like intersperse
note that if you want to try out intersperse you need to

import Data.List
-}
thintersperse :: a -> ThList a -> ThList a
thintersperse x Nil = Cons Nil x
thintersperse x (Cons Nil y) = Cons Nil y
thintersperse x (Cons (Cons Nil z) y) = Cons (Cons (Cons Nil z) x) y
thintersperse x (Cons ys y) = Cons (Cons (thintersperse x ys) x) y
{- thconcat is like concat -}
-- thconcat :: ThList(ThList a) -> ThList a
 --thconcat Nil = Nil
 --thconcat (Cons Nil (Cons Nil)) = Cons  
thconcat :: ThList(ThList a) -> ThList a
thconcat Nil = Nil
thconcat (Cons _ Nil) = Nil
thconcat (Cons x y) = thappend (thappend y Nil) (thconcat x)