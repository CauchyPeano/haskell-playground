  import Text.Parsec

  ignoreBraces :: Parsec [Char] u a -> Parsec [Char] u b -> Parsec [Char] u c -> Parsec [Char] u c
  ignoreBraces a b x = a *> x <* b

  test = ignoreBraces (string "[[") (string "]]") (many1 letter)
