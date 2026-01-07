module Control.Arrow
  ( Arrow (..),
    Kleisli (..),
    returnA,
    (^>>),
    (>>^),
    (<<^),
    (^<<),
    ArrowZero (..),
    ArrowChoice (..),
    ArrowApply (..),
    ArrowMonad (..),
    leftApp,
    ArrowLoop (..),
  )
where

import Control.Category
  ( Category (id, (.)),
    (<<<),
    (>>>),
  )
import Prelude hiding (id, (.))

class (Category a) => Arrow a where
  arr :: (b -> c) -> a b c
  first :: a b c -> a (b, d) (c, d)
  second :: a b c -> a (d, b) (d, c)
  infixr 3 ***
  (***) :: a b c -> a b' c' -> a (b, b') (c, c')
  infixr 3 &&&
  (&&&) :: a b c -> a b c' -> a b (c, c')

newtype Kleisli m a b = Kleisli
  { runKleisli :: a -> m b
  }

returnA :: (Arrow a) => a b b
returnA = error "todo"

infixr 1 ^>>

(^>>) :: (Arrow a) => (b -> c) -> a c d -> a b d
(^>>) = error "todo"

infixr 1 >>^

(>>^) :: (Arrow a) => a b c -> (c -> d) -> a b d
(>>^) = error "todo"

infixr 1 <<^

(<<^) :: (Arrow a) => a c d -> (b -> c) -> a b d
(<<^) = error "todo"

infixr 1 ^<<

(^<<) :: (Arrow a) => (c -> d) -> a b c -> a b d
(^<<) = error "todo"

class (Arrow a) => ArrowZero a where
  zeroArrow :: a b c

class (ArrowZero a) => ArrowPlus a where
  infixr 5 <+>
  (<+>) :: a b c -> a b c -> a b c

class (Arrow a) => ArrowChoice a where
  left :: a b c -> a (Either b d) (Either c d)
  right :: a b c -> a (Either d b) (Either d c)
  infixr 2 +++, |||
  (+++) :: a b c -> a b' c' -> a (Either b b') (Either c c')
  (|||) :: a b d -> a c d -> a (Either b c) d

class (Arrow a) => ArrowApply a where
  app :: a (a b c, b) c

newtype ArrowMonad a b = ArrowMonad (a () b)

leftApp :: (ArrowApply a) => a b c -> a (Either b d) (Either c d)
leftApp = error "todo"

class (Arrow a) => ArrowLoop a where
  loop :: a (b, d) (c, d) -> a b c
