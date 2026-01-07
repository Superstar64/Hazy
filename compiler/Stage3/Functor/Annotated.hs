module Stage3.Functor.Annotated where

import Data.Bifoldable (Bifoldable (..))
import Data.Bifunctor (Bifunctor (..), second)
import Data.Bitraversable (Bitraversable (..), bifoldMapDefault, bimapDefault)
import Data.Kind (Type)
import Stage2.Scope (Environment)

type Annotated :: (Environment -> Type) -> Type -> Type -> Type
data Annotated label a b = Annotated
  { label :: !(forall scope. label scope),
    meta :: a,
    content :: b
  }

type NoLabel :: Environment -> Type
data NoLabel scope = NoLabel

instance Functor (Annotated name a) where
  fmap = second

instance Bifunctor (Annotated name) where
  bimap = bimapDefault

instance Bifoldable (Annotated name) where
  bifoldMap = bifoldMapDefault

instance Bitraversable (Annotated name) where
  bitraverse f g Annotated {label, meta, content} =
    Annotated label <$> f meta <*> g content
