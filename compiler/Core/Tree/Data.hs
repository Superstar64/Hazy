module Core.Tree.Data where

import qualified Core.Substitute as Substitute
import Core.Tree.Constructor (ConstructorF)
import Core.Tree.Type (Type, (-#>))
import qualified Core.Tree.Type as Type
import qualified Data.Vector.Strict as Strict
import Data.Void (Void)
import Semantic.Scope (Environment ((:+)), Local)
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Semantic.Tree.Selector (Selector)
import Syntax.Tree.Brand (Brand)

data Data scope = Data
  { parameters :: !(Strict.Vector (Type scope)),
    definition :: !(Definition (Local ':+ scope))
  }
  deriving (Show)

instance Shift0.Functor Data where
  map = Shift.mapDefault

instance Shift.Functor Data where
  map = Substitute.mapDefault

instance Substitute.Functor Data where
  map category Data {parameters, definition} =
    Data
      { parameters = Substitute.map category <$> parameters,
        definition = Substitute.map (Substitute.Over category) definition
      }

type Definition = DefinitionF Void

data DefinitionF logical scope = Definition
  { constructors :: !(Strict.Vector (ConstructorF logical scope)),
    selectors :: !(Strict.Vector Selector),
    brand :: !Brand
  }
  deriving (Show)

instance Shift0.Functor (DefinitionF logical) where
  map = Shift.mapDefault

instance Shift.Functor (DefinitionF logical) where
  map category Definition {constructors, selectors, brand} =
    Definition
      { constructors = Shift.map category <$> constructors,
        selectors,
        brand
      }

instance (logical ~ Void) => Substitute.Functor (DefinitionF logical) where
  map category Definition {constructors, selectors, brand} =
    Definition
      { constructors = Substitute.map category <$> constructors,
        selectors,
        brand
      }

kind :: Data scope -> Type scope
kind Data {parameters} = foldr (-#>) Type.typex parameters
