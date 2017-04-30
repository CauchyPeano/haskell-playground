unCmps3 :: Functor f => (f |.| g |.| h) a -> f (g (h a))
unCmps3 = undefined

unCmps4 :: (Functor f2, Functor f1) => (f2 |.| f1 |.| g |.| h) a -> f2 (f1 (g (h a)))
unCmps4 = undefined
