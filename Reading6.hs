--Exercise 8.1
data Nat = Zero | Succ Nat
add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)
mult :: Nat -> Nat -> Nat
mult Zero n = Zero
mult (Succ m) n = add m (mult m n)

--Exercise 8.3
data BTree a = BLeaf a | BNode (BTree a) (BTree a)

leaves :: BTree a -> Int
leaves (BLeaf _)    = 1
leaves (BNode l r)  = (leaves l) + (leaves r)

balanced :: BTree a -> Bool
balanced (BLeaf y)   = True
balanced (BNode l r) = abs (leaves l - leaves r) <= 1 && balanced l && balanced r

halves :: [a] -> ([a],[a])
halves list = (take half list, drop half list)
              where half = (length list `div` 2)
--Exercise 8.5
data Expr = Val Int | Add Expr Expr
folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g e = case e of
  Val a -> f a
  Add a b -> g (folde f g a) (folde f g b)

--Exercise 8.7
data Maybe' a = Anything | Only a
instance Eq a => Eq (Maybe' a) where
  Only x == Only y = x == y
  Anything == Anything = True
  _ == _ = False

{-
data List' a = Nil | Cons a (List' a)
instance Eq a => Eq (List' a)  where
  [] == [] = True
  (x:xs) == (y:ys) = x == y && xs == ys
  _ == _ = False
-}
