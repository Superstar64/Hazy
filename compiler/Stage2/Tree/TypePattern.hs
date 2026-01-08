module Stage2.Tree.TypePattern where

import qualified Stage1.Tree.TypePattern as Stage1
import Stage1.Variable (VariableIdentifier)

data TypePattern position = TypePattern
  { position :: !position,
    name :: !VariableIdentifier
  }
  deriving (Show)

-- Equality must ignore variable names
instance (Eq position) => Eq (TypePattern position) where
  TypePattern {position} == TypePattern {position = position'} =
    position == position'

anonymize :: TypePattern position -> TypePattern ()
anonymize TypePattern {name} =
  TypePattern
    { position = (),
      name
    }

resolve :: Stage1.TypePattern position -> TypePattern position
resolve Stage1.TypePattern {position, name} =
  TypePattern {position, name}
