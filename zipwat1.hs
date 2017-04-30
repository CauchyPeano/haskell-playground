import Control.Applicative (ZipList(ZipList), getZipList)

infixl 4 >$<, >*<
(>$<) = (<$>)
(>*<) = zipWith ($)
