newtype Prs a = Prs { runPrs :: String -> Maybe (a, String) }

instance Functor Prs where
  fmap f p = Prs fun where
--    fun s = [ (f a, s') | (a, s') <- runPrs p s]
    fun s = case (runPrs p s) of
      Nothing -> Nothing
      Just (a, b) -> Just (f a, b)


anyChr :: Prs Char
anyChr = Prs f where
  f "" = Nothing
  f (x:xs) = Just (x, xs)
