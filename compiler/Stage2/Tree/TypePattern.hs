module Stage2.Tree.TypePattern where

import qualified Stage1.Tree.TypePattern as Stage1
import Stage1.Variable (VariableIdentifier)
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Stage (Check, Resolve)
import Stage2.Tree.Combinators.Inferred (Inferred (..), get)
import {-# SOURCE #-} Stage4.Tree.Type as Simple (Type)

data TypePattern position stage scope = TypePattern
  { position :: !position,
    name :: !VariableIdentifier,
    typex :: !(Inferred Simple.Type stage scope)
  }
  deriving (Show)

-- Equality must ignore variable names
instance (Eq position) => Eq (TypePattern position stage scope) where
  TypePattern {position} == TypePattern {position = position'} =
    position == position'

instance Shift (TypePattern position stage) where
  shift = shiftDefault

instance Shift.Functor (TypePattern position stage) where
  map category TypePattern {position, name, typex} =
    TypePattern
      { position,
        name,
        typex = Shift.map category typex
      }

anonymize :: TypePattern position stage scope -> TypePattern () stage scope
anonymize TypePattern {name, typex} =
  TypePattern
    { position = (),
      name,
      typex
    }

resolve :: Stage1.TypePattern position -> TypePattern position Resolve scope
resolve Stage1.TypePattern {position, name} =
  TypePattern
    { position,
      name,
      typex = Inferred
    }

typex' :: TypePattern position Check scope -> Simple.Type scope
typex' = get . typex
