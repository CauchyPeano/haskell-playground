data Triple a = Tr a a a
  deriving (Eq,Show)

instance Foldable Triple where
  foldr f ini (Tr a b c) = f a $ f b $ f c ini
  foldl f ini (Tr a b c) = f (f (f ini a) b) c

instance Functor Triple where
  fmap f (Tr x y z) = Tr (f x) (f y) (f z)

instance Traversable Triple where
  --traverse :: Applicative f => (a -> f b) -> Triple a -> f (Triple b)
  traverse g (Tr x y z) = pure Tr <*> g x <*> g y <*> g z
