import Control.Applicative

traverse2list :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f [b]
traverse2list fun = foldr (\x y -> pure (:) <*> fun x <*> y) (pure []) 
  -- stun :: a -> f [b] -> f [b]
  -- stun =
