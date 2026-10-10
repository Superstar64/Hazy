module Core.Tree.Data where

import Core.Instanciate (Normal, Store)
import qualified Core.Substitute as Substitute
import Core.Tree.Combinators.Delay (Delay)
import Core.Tree.Constructor (ConstructorF)
import Core.Tree.Type (Type, TypeF, (-#>))
import qualified Core.Tree.Type as Type
import qualified Core.Type.Show as Core
import qualified Data.Vector.Strict as Strict
import Data.Void (Void)
import Semantic.Scope (Environment ((:+)), Local)
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Semantic.Tree.Selector (Selector)
import Syntax.Position (Position)
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

type Definition = DefinitionF Normal Void

newtype Types logical scope = Types (Strict.Vector (TypeF logical scope))
  deriving (Show)

instance Core.Show Types where
  showsPrec = showsPrec

instance Shift0.Functor (Types logical) where
  map = Shift.mapDefault

instance Shift.Functor (Types logical) where
  map category (Types types) = Types (Shift.map category <$> types)

instance (logical ~ Void) => Substitute.Functor (Types logical) where
  map category (Types types) = Types (Substitute.map category <$> types)

data DefinitionF instanciate logical scope = Definition
  { position :: !(Store instanciate Position),
    types :: !(Delay Types instanciate logical scope),
    constructors :: !(Strict.Vector (ConstructorF instanciate logical scope)),
    selectors :: !(Strict.Vector Selector),
    brand :: !Brand
  }
  deriving (Show)

instance Shift0.Functor (DefinitionF instanciate logical) where
  map = Shift.mapDefault

instance Shift.Functor (DefinitionF instanciate logical) where
  map category Definition {position, types, constructors, selectors, brand} =
    Definition
      { position,
        types = Shift.map category types,
        constructors = Shift.map category <$> constructors,
        selectors,
        brand
      }

instance (logical ~ Void) => Substitute.Functor (DefinitionF instanciate logical) where
  map category Definition {position, types, constructors, selectors, brand} =
    Definition
      { position,
        types = Substitute.map category types,
        constructors = Substitute.map category <$> constructors,
        selectors,
        brand
      }

kind :: Data scope -> Type scope
kind Data {parameters} = foldr (-#>) Type.typex parameters
