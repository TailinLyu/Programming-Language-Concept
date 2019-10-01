--Exercise 12.1
data Tree a = Leaf | Node (Tree a) a (Tree a) 
  deriving Show
instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap g Leaf = Leaf
  fmap g (Node l a r) = Node (fmap g l) (g a) (fmap g r)

--Exercise 12.4
newtype ZipList a = Z [a] deriving Show
instance Functor ZipList where
-- fmap :: (a -> b) -> ZipList a -> ZipList b
  fmap g (Z xs) = Z (fmap g xs)
instance Applicative ZipList where
-- pure :: a -> ZipList a
  pure x = Z (repeat x)
-- <*> :: ZipList (a -> b) -> ZipList a -> ZipList b
  (Z gs) <*> (Z xs) = Z [g x | (g,x) <- zip gs xs]

--Exercise 12.7
data Expr a = Var a | Val Int | Add (Expr a) (Expr a)
  deriving Show
instance Functor Expr where
  fmap g (Var a) = Var (g a)
  fmap g (Val x) = Val x
  fmap g (Add x y) = Add (fmap g x) (fmap g y)