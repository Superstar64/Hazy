module Semantic.Resolve.Binding.Constructor where

import Data.Functor.Classes (Show1 (..), showsPrec1)
import Data.Functor.Compose (Compose (..))
import Data.Functor.Identity (Identity)
import Data.Map (Map)
import Data.NaturalTransformation (NaturalTransformation (..))
import qualified Data.Strict.Maybe as Strict (Maybe)
import qualified Data.Vector.Strict as Strict (Vector)
import Error (duplicateConstructorEntries)
import qualified Semantic.Index.Constructor as Constructor
import Semantic.Resolve.Functor2 (Traversable2 (..))
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Syntax.Position (Position)
import Syntax.Tree.Fixity (Fixity)
import Syntax.Variable (Variable)

type Binding = BindingF Identity

data BindingF loeb scope
  = (:@)
  { position :: !Position,
    value :: loeb (Detail scope)
  }

instance (Show1 loeb) => Show (BindingF loeb scope) where
  showsPrec d (position :@ binding) =
    showParen (d > 9) $
      showsPrec 10 position . showString " :@ " . showsPrec1 10 binding

instance (Functor loeb) => Shift0.Functor (BindingF loeb) where
  map = Shift.mapDefault

instance (Functor loeb) => Shift.Functor (BindingF loeb) where
  map category (position :@ binding) = position :@ fmap (Shift.map category) binding

instance (Applicative loeb) => Semigroup (BindingF loeb scope) where
  position :@ binding <> position2 :@ binding' =
    position :@ (liftA2 (combine position position2) binding binding')

instance Traversable2 BindingF where
  traverse2 (Morph f) (position :@ binding) = (:@) position <$> getCompose (f binding)

data Detail scope = Binding
  { index :: !(Constructor.Index scope),
    fixity :: !Fixity,
    fields :: !(Map Variable Int),
    -- redirect a selector index into an entry index
    selections :: !(Strict.Vector (Strict.Maybe Int)),
    unordered :: !Bool,
    fielded :: !Bool,
    single :: !Bool
  }
  deriving (Show)

combine position1 position2 binding@Binding {index = index1} Binding {index = index2}
  | index1 == index2 = binding
  | otherwise = duplicateConstructorEntries [position1, position2]

instance Shift0.Functor Detail where
  map = Shift.mapDefault

instance Shift.Functor Detail where
  map category Binding {index, fixity, fields, selections, unordered, fielded, single} =
    Binding
      { index = Shift.map category index,
        fixity,
        fields,
        selections,
        unordered,
        fielded,
        single
      }
