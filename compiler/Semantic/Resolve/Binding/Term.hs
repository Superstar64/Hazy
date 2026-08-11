module Semantic.Resolve.Binding.Term where

import Data.Functor.Classes (Show1 (..), showsPrec1)
import Data.Functor.Compose (Compose (..))
import Data.Functor.Identity (Identity)
import Data.NaturalTransformation (NaturalTransformation (..))
import Error (duplicateVariableEntries)
import qualified Semantic.Index.Selector as Selector
import qualified Semantic.Index.Term2 as Term2
import Semantic.Resolve.Functor2 (Traversable2 (..))
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Syntax.Position (Position)
import Syntax.Tree.Fixity (Fixity)

type Binding = BindingF Identity

data BindingF m scope = (:@)
  { position :: !Position,
    value :: m (Detail scope)
  }

instance (Show1 m) => Show (BindingF m scope) where
  showsPrec d (position :@ binding) =
    showParen (d > 9) $
      showsPrec 10 position . showString " :@ " . showsPrec1 10 binding

instance (Functor m) => Shift0.Functor (BindingF m) where
  map = Shift.mapDefault

instance (Functor m) => Shift.Functor (BindingF m) where
  map category (position :@ binding) = position :@ fmap (Shift.map category) binding

instance (Applicative m) => Semigroup (BindingF m scope) where
  position :@ binding <> position2 :@ binding' =
    position :@ (liftA2 (combine position position2) binding binding')

instance Traversable2 BindingF where
  traverse2 (Morph f) (position :@ binding) = (:@) position <$> getCompose (f binding)

data Detail scope = Binding
  { index :: !(Term2.Index scope),
    fixity :: !Fixity,
    selector :: Selector scope
  }
  deriving (Show)

combine
  position1
  position2
  Binding {index = index1, fixity, selector = selector1}
  Binding {index = index2, selector = selector2}
    | index1 == index2 =
        Binding
          { index = index1,
            fixity,
            selector = selector1 <> selector2
          }
    | otherwise = duplicateVariableEntries [position1, position2]

instance Shift0.Functor Detail where
  map = Shift.mapDefault

instance Shift.Functor Detail where
  map category Binding {index, fixity, selector} =
    Binding
      { index = Shift.map category index,
        fixity,
        selector = Shift.map category selector
      }

data Selector scope
  = Selector !(Selector.Index scope)
  | Normal
  deriving (Show)

instance Shift0.Functor Selector where
  map = Shift.mapDefault

instance Shift.Functor Selector where
  map category = \case
    Selector index -> Selector (Shift.map category index)
    Normal -> Normal

instance Semigroup (Selector scope) where
  field@Selector {} <> _ = field
  Normal <> field = field

instance Monoid (Selector scope) where
  mempty = Normal
