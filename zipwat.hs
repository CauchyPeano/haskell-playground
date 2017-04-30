import Control.Applicative (ZipList(ZipList), getZipList)

x1s = [1,2,3]
x2s = [4,5,6]
x3s = [7,8,9]
x4s = [10,11,12]

infixl 4 >$<
(>$<) :: (a -> b) -> [a] -> [b]
f >$< as = getZipList $ (f <$> ZipList as)

infixl 4 >*<
(>*<) :: [a -> b] -> [a] -> [b]
fs >*< as = getZipList $ ZipList fs <*> ZipList as

--woot = (\a b -> 2*a+3*b) >$< x1s >*< x2s
