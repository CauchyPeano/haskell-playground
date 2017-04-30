data Triple a = Tr a a a deriving (Eq,Show)

instance Functor Triple where
  fmap f (Tr a b c) = Tr (f a) (f b) (f c)

instance Applicative Triple where
  pure x = Tr x x x
  (Tr f g h) <*> (Tr a b c) = Tr (f a) (g b) (h c)
