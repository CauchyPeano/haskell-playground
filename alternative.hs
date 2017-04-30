import Control.Applicative
import Data.Char

newtype Prs a = Prs { runPrs :: String -> Maybe (a, String) }

instance Alternative Prs where
  empty = Prs $ \s -> Nothing
  p <|> q = Prs fun where
    fun s = case (runPrs p s) of
      Nothing -> runPrs q s
      Just p -> Just p

instance Functor Prs where
  fmap f (Prs p) = Prs fun where
    fun = fmap (fmap pf) p where
      pf (a, b) = (f a, b)

instance Applicative Prs where
  pure a = Prs fun where
    fun s = Just (a, s)

  (<*>) pf pv = Prs fun where
    fun s = case (runPrs pf s) of
      Just (f, s') -> (fmap (fmap pf) (runPrs pv)) s'
        where pf (a, b) = (f a, b)
      Nothing -> Nothing

satisfy :: (Char -> Bool) -> Prs Char
satisfy f = Prs fun where
  fun "" = Nothing
  fun (x:xs) = if (f x) then Just (x, xs) else Nothing

char :: Char -> Prs Char
char c = satisfy (== c)

mmany :: Prs a -> Prs [a]
mmany p = (:) <$> p <*> mmany p <|> pure []

many1 :: Prs a -> Prs [a]
many1 p = (:) <$> (p <|> empty) <*> many p

nat :: Prs Int
nat = read <$> (many1 (satisfy isDigit))

mult :: Prs Int
mult = (*) <$> nat <* char '*' <*> nat
