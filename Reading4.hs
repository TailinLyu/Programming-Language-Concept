--Exercise 4.1
halve :: [a] -> ([a],[a])
halve xs = (take((length xs) `div` 2) xs,drop ((length xs) `div` 2) xs)

--Exercise 4.2
third :: [a] -> a
third xs = head (tail (tail xs))

third' :: [a] -> a
third' xs = xs !! 2

third'' :: [a] -> a
third'' (_:_:x:_) = x

--Exercise 4.3
safetail :: [a] -> [a]
safetail xs = if null xs then [] else
  tail xs

safetail' :: [a] -> [a]
safetail' xs  |null xs =  []
              | otherwise = tail xs

safetail'' :: [a] -> [a]
safetail'' [] = []
safetail'' (_:xs) = tail xs

--Exercise 4.4
False || False = False
_ || _ = True

{-
(||) :: Bool -> Bool -> Bool
True || True = True
True || False = True
False || True = True
False || False = False

True || b = b
False || _ = False

a || b |a == b = a
       |otherwise = True
       -}

--Exercise 4.5
doubleAnd :: Bool -> Bool -> Bool
doubleAnd a b =
  if (a == True)
     then if (b == True) then True
                              else False
  else False

--Exercise 4.6
tb :: Bool -> Bool -> Bool
tb x y = if x then y else False

a && b = if a == True then b
  else False
--Exercise 4.7
mult :: Int -> Int -> Int -> Int
mult x y = \z -> x * y
  * z

mult' :: Int -> Int -> Int -> Int
mult' x = \y z -> x * y * z

foo :: Int -> Int ->Int -> Int
foo b c = \a -> a + b * c
