module Control.Category
  ( Category (..),
    (<<<),
    (>>>),
  )
where

import Hazy (placeholder)
import Prelude hiding (id, (.))

class Category cat where
  id :: cat a a
  infixr 9 .
  (.) :: cat b c -> cat a b -> cat a c

infixr 1 <<<

(<<<) :: (Category cat) => cat b c -> cat a b -> cat a c
(<<<) = placeholder

infixr 1 >>>

(>>>) :: (Category cat) => cat a b -> cat b c -> cat a c
(>>>) = placeholder
