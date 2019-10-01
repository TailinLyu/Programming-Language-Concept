--Exercise 5.1
x1 = sum[x^2 | x <- [1..100]]

--Exercise 5.4
replicate' :: Int -> a -> [a]
replicate' n x = [x | _ <- [1..n]]

--Exercise 5.2
grid :: Int -> Int -> [(Int,Int)]
grid a b = [(x,y) | x <-[0..a], y <-[0..b]]

--Exercise 5.6
factors :: Int -> [Int]
factors x = [a | a <- [1..(x-1)], x `mod` a == 0]
perfect :: Int -> [Int]
perfect x = [a | a <- [2..x], (sum(factors a)) == a]

--Exercise 6.1
fac' :: Int -> Int
fac' 0 = 1
fac' n | n > 0 = n * fac'(n-1)

--Exercise 6.3
exp' :: Int -> Int -> Int
exp' x 0 = 1
exp' x y = x * (exp' x (y-1))

--Exercise 6.6a
and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs

--Exercise 6.6b
concat' :: [[a]] -> [a]
concat' [x] = x
concat' (x:xs) = x ++ (concat' xs)

--Exercise 7.1
--map (f)(filter p x)

--Exercise 7.2c
takewhile :: (a -> Bool) -> [a] -> [a]
takewhile _ [] = []
takewhile p (x:xs) | p x = x :takewhile p xs
                   | otherwise = []

--Exercise 7.3
map' f = foldr (\x xs -> f x : xs) []
filter' p = foldr (\x xs -> if p x then x:xs else xs) []

--Exercise 7.9
altMap :: (a->b) -> (a->b) -> [a] -> [b]
altMap f p xs = map (\(x,y) -> if odd y then p x else f x) (zip xs [0..])
