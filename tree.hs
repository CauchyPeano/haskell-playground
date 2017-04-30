data Tree a = Nil | Branch (Tree a) a (Tree a)   deriving (Eq, Show)

testTree = Branch (Branch (Branch Nil 1 Nil) 2 (Branch Nil 3 Nil)) 4 (Branch Nil 5 Nil)

tt = Branch (Branch (Branch Nil 'A' Nil) 'B' (Branch (Branch Nil 'C' Nil) 'D' (Branch Nil 'E' Nil))) 'F' (Branch Nil 'G' (Branch (Branch Nil 'H' Nil) 'I' Nil))

instance Foldable Tree where
  foldr f ini Nil = ini
  foldr f ini (Branch l x r) = foldr f (f x (foldr f ini r)) l

instance Functor Tree where
  fmap _ Nil = Nil
  fmap f (Branch l a r) = Branch (fmap f l) (f a) (fmap f r)

instance Applicative Tree where
  pure a = Branch (pure a) a (pure a)
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (Branch fl fa fr) <*> (Branch l x r) = Branch (fl <*> l) (fa x) (fr <*> r)

instance Traversable Tree where
  traverse _ Nil = pure Nil
  traverse g (Branch l a r) = Branch <$> traverse g l <*> g a <*> traverse g r

treeToList :: Tree a -> [a]
treeToList t = foldr (:) [] t



newtype Postorder a  = PostO  (Tree a)    deriving (Eq, Show)
newtype Preorder a   = PreO   (Tree a)    deriving (Eq, Show)
newtype Levelorder a = LevelO (Tree a)    deriving (Eq, Show)

instance Foldable Preorder where
  foldr f ini (PreO Nil) = ini
  foldr f ini (PreO (Branch l x r)) = f x (foldr f (foldr f ini (PreO r)) (PreO l))

instance Foldable Postorder where
  foldr _ ini (PostO Nil) = ini
  foldr f ini (PostO (Branch l x r)) = foldr f (foldr f (f x ini) (PostO r)) (PostO l)

instance Foldable Levelorder where
  foldr _ ini (LevelO Nil) = ini
  foldr f ini (LevelO t) = foldr f ini (traverse' t)

traverse' :: Tree a -> [a]
traverse' tree = tbf [tree]
    where
        tbf [] = []
        tbf xs = map nodeValue xs ++ tbf (concat (map leftAndRightNodes xs))

        nodeValue (Branch _ x _ ) = x

        leftAndRightNodes (Branch Nil _ Nil) = []
        leftAndRightNodes (Branch Nil _ r)   = [r]
        leftAndRightNodes (Branch l _ Nil)   = [l]
        leftAndRightNodes (Branch l _ r)     = [l,r]
