module Data.List.Reverse where

data List a = Nil | List a :> a
  deriving (Show)

infixl 5 :>

last (_ :> last) = last
last _ = error "empty list"

init (init :> _) = init
init _ = error "empty list"

instance Foldable List where
  foldr cons nil = \case
    Nil -> nil
    init :> last -> foldr cons (cons last nil) init
  foldl snoc lin = \case
    Nil -> lin
    init :> last -> snoc (foldl snoc lin init) last

instance Functor List where
  fmap _ Nil = Nil
  fmap f (list :> head) = fmap f list :> f head

fromList :: [a] -> List a
fromList = foldl (:>) Nil
