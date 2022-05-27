data Tree 𝑎 = Leaf 𝑎 | Branch (Tree 𝑎) (Tree 𝑎)
  deriving (Show)

instance Functor Tree where
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Branch l r) = Branch (fmap f l) (fmap f r)

t = Branch (Leaf "a") (Branch (Leaf "b") (Leaf "c"))

number (Leaf a) s = (Leaf s, s + 1)
number (Branch l r) s =
  let (l', s') = number l s
      (r', s'') = number r s'
   in (Branch l' r', s'')

-- zipTree :: Tree 𝛼 -> Tree 𝛽 -> Tree (𝛼, 𝛽)
-- zipTree (Leaf a) (Leaf b) = Leaf (a, b)
-- zipTree (Branch l r) (Branch l' r') =
--   Branch (zipTree l l') (zipTree r r')

ret :: 𝛼 -> Maybe 𝛼
ret x = Just x

bind :: Maybe 𝛼 -> (𝛼 -> Maybe 𝛽) -> Maybe 𝛽
bind x f =
  case x of
    Nothing -> Nothing
    Just l'' -> f l''

zipTree :: Tree 𝛼 -> Tree 𝛽 -> Maybe (Tree (𝛼, 𝛽))
zipTree (Leaf a) (Leaf b) = ret (Leaf (a, b))
zipTree (Branch l r) (Branch l' r') =
  zipTree l l' `bind` \l'' ->
  zipTree r r' `bind` \r'' ->
     ret (Branch l'' r'')
zipTree _ _ = Nothing