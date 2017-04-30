newtype PrsE a = PrsE { runPrsE :: String -> Either String (a, String) }

satisfyE :: (Char -> Bool) -> PrsE Char
satisfyE f = PrsE fun where
  fun "" = Left "unexpected end of input"
  fun (x:xs) = if (f x) then Right (x, xs) else Left ( "unexpected " ++ (x : ""))

charE :: Char -> PrsE Char
charE c = satisfyE (== c)

anyE = satisfyE (const True)

instance Functor PrsE where
  fmap f p = PrsE fun where
    fun = fmap (fmap pf) (runPrsE p) where
      pf (a, str) = (f a, str)

instance Applicative PrsE where
  pure a = PrsE fun where
    fun s = Right (a, s)
  pf <*> pv = PrsE fun where
    fun s = case (runPrsE pf s) of
      Right (a, s') -> runPrsE (fmap a pv) s'
      Left s' -> Left s'
