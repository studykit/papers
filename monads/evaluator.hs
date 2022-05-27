data Term = Con Int | Div Term Term
  deriving (Eq, Show)

eval :: (Monad m) => Term -> m Int
eval (Con a) = return a
eval (Div t u) =
  eval t >>= \a ->
  eval u >>= \b ->
  return (div a b)
