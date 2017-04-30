newtype Prs a = Prs { runPrs :: String -> Maybe (a, String) }

instance Functor Prs where
  fmap f (Prs p) = Prs fun where
    fun = fmap (fmap pf) p where
      pf (a, b) = (f a, b)

anyChr :: Prs Char
anyChr  = Prs f where
  f ""     = Nothing
  f (c:cs) = Just (c, cs)

instance Applicative Prs where
  pure a = Prs fun where
    fun s = Just (a, s)

  (<*>) pf pv = Prs fun where
    fun s = case (runPrs pf s) of
      Just (f, s') -> (fmap (fmap pf) (runPrs pv)) s'
        where pf (a, b) = (f a, b)
      Nothing -> Nothing
