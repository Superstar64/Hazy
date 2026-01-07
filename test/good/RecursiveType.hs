module RecursiveType where

import Data.Kind (Type)

type List :: Type -> Type
data List a = Nil | Cons a (List a)
