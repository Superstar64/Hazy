module Semantic.Resolve.Binding.Type where

import Data.Functor.Classes (Show1 (..))
import Data.Functor.Compose (Compose (..))
import Data.Functor.Identity (Identity)
import Data.Map (Map)
import Data.NaturalTransformation (NaturalTransformation (..))
import Data.Set (Set)
import Error (duplicateTypeEntries)
import qualified Semantic.Index.Type3 as Type3
import Semantic.Resolve.Functor2 (Traversable2 (..))
import Semantic.Shift (Shift (..), shiftDefault)
import qualified Semantic.Shift as Shift
import Syntax.Position (Position)
import Syntax.Variable (Constructor, Variable)

type Binding = BindingF Identity

data BindingF m scope = (:@)
  { header :: !Header,
    value :: m (Detail scope)
  }

instance (Show1 m) => Show (BindingF m scope) where
  showsPrec d (header :@ binding) =
    showParen (d > 9) $
      showsPrec 10 header . showString " :@ " . liftShowsPrec showsPrec showList 10 binding

instance (Functor m) => Shift (BindingF m) where
  shift = shiftDefault

instance (Functor m) => Shift.Functor (BindingF m) where
  map category (header :@ binding) = header :@ fmap (Shift.map category) binding

instance (Applicative m) => Semigroup (BindingF m scope) where
  Header
    { position,
      constructors = constructors1,
      fields = fields1
    }
    :@ binding1
    <> Header
      { position = position2,
        constructors = constructors2,
        fields = fields2
      }
      :@ binding2 =
      Header
        { position,
          constructors = constructors1 <> constructors2,
          fields = fields1 <> fields2
        }
        :@ liftA2 (combine position position2) binding1 binding2

instance Traversable2 BindingF where
  traverse2 (Morph f) (header :@ binding) = (:@) header <$> getCompose (f binding)

data Header = Header
  { position :: !Position,
    constructors :: !(Set Constructor),
    fields :: !(Set Variable)
  }
  deriving (Show)

data Detail scope = Binding
  { index :: !(Type3.Index scope),
    methods :: !(Map Variable Int)
  }
  deriving (Show)

combine
  position1
  position2
  left@Binding {index = index1}
  Binding {index = index2}
    | index1 == index2 =
        left
    | otherwise = duplicateTypeEntries [position1, position2]

instance Shift Detail where
  shift = shiftDefault

instance Shift.Functor Detail where
  map category Binding {index, methods} =
    Binding
      { index = Shift.map category index,
        methods
      }
