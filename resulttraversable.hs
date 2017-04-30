data Result a = Ok a | Error String deriving (Eq,Show)

instance Functor Result where
  fmap _ (Error str) = Error str
  fmap f (Ok a) = Ok $ f a

instance Applicative Result where
  pure = Ok
  (Error str) <*> _ = Error str
  (Ok f) <*> res = fmap f res

instance Foldable Result where
  foldMap _ (Error str) = mempty
  foldMap f (Ok a) = f a

instance Traversable Result where
  traverse _ (Error str) = pure (Error str)
  traverse g (Ok a) = Ok <$> g a
