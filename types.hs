{-# LANGUAGE TypeOperators #-}
import Data.Foldable

infixr 9 |.|
newtype (|.|) f g a = Cmps {getCmps :: f (g a)}
  deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (f |.| g) where
    fmap z (Cmps x) = Cmps $ fmap (fmap z) x

instance (Applicative f, Applicative g) => Applicative (f |.| g) where
    pure = Cmps . pure . pure
    pv <*> pu = undefined

instance (Traversable f, Traversable g) => Traversable (f |.| g) where
--  traverse f (Cmps fga) = undefined
    traverse f c = pure Cmps <*> dd where
      dd = traverse fun fga
      fun = traverse f
      fga = getCmps c


type A   = ((,) Integer |.| (,) Char) Bool
type B t = ((,,) Bool (t -> t) |.| Either String) Int
type C   = (|.|) ((->) Bool) ((->) Integer) Integer

a :: A
a = Cmps (1 :: Integer, ('x', True))

b :: B t
b = Cmps (True, \a -> a, Right 1 :: Either String Int)

c :: C
c  = Cmps fun where
  fun :: Bool -> (Integer -> Integer)
  fun b = \i -> (i + 1)


unCmps3 :: Functor f => ((|.|) f (g |.| h)) a -> f (g (h a))
unCmps3 x = fmap getCmps (getCmps x)

unCmps4 :: (Functor f2, Functor f1) => (|.|) f2 (f1 |.| (g |.| h)) a -> f2 (f1 (g (h a)))
unCmps4 x = fmap (fmap getCmps) (fmap getCmps (getCmps x))

instance (Foldable f, Foldable g) => Foldable (f |.| g) where
  --  foldMap :: (a -> m) -> (|.|) f g a -> m
  -- foldMap fam (Cmps x) = foldMap fam x
  foldr f ini cont = foldr f ini as where
    fga = getCmps cont
    gas = toList fga
    as = concat $ map toList gas

  -- foldr :: (a -> b -> b) -> b -> (|.|) f g a -> b
  -- cont :: (|.|) f g a (bound at types.hs:40:15)
  -- f :: a -> b -> b (bound at types.hs:40:9)

-- fam :: a -> m
-- x :: f (g a)
-- foldMap fam x ::
