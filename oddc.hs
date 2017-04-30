data OddC a = Un a | Bi a a (OddC a) deriving (Eq,Show)

instance Functor OddC where
  fmap f (Un a) = Un $ f a
  fmap f (Bi a b oddc) = Bi (f a) (f b) (fmap f oddc)

instance Foldable OddC where
  foldr f ini (Un a) = f a ini
  foldr f ini (Bi a b oddc) = f a . f b $ foldr f ini oddc

-- instance Applicative OddC where
--   pure a = Bi a a (pure a)
--   pv <*> pa = pure Bi

instance Traversable OddC where
  sequenceA (Un a) = pure Un <*> a
  sequenceA (Bi a b oddc) = Bi <$> a <*> b <*> (sequenceA oddc)

cnt1 = Un 42
cnt3 = Bi 1 2 cnt1
cnt5 = Bi 3 4 cnt3
cntInf = Bi 'A' 'B' cntInf
