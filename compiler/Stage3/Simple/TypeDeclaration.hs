module Stage3.Simple.TypeDeclaration where

import Stage1.Lexer (ConstructorIdentifier)
import qualified Stage2.Index.Term as Term
import Stage2.Scope (Environment ((:+)), Local)
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Stage3.Simple.Class (Class)
import qualified Stage3.Simple.Class as Class
import qualified Stage3.Simple.Constraint as Constraint
import qualified Stage3.Simple.Constructor as Constructor
import Stage3.Simple.Data (Data)
import qualified Stage3.Simple.Data as Data
import Stage3.Simple.Type (Type)
import qualified Stage3.Tree.Method as Solved.Method
import qualified Stage3.Tree.TypeDeclaration as Solved

data TypeDeclaration scope
  = Data
      { name :: !ConstructorIdentifier,
        datax :: !(Data scope)
      }
  | Class
      { name :: !ConstructorIdentifier,
        classx :: !(Class scope)
      }
  | Synonym
      { name :: !ConstructorIdentifier,
        definition :: !(Type (Local ':+ scope))
      }
  deriving (Show)

assumeData :: TypeDeclaration scope -> Data scope
assumeData Data {datax} = datax
assumeData _ = error "not data"

assumeClass :: TypeDeclaration scope -> Class scope
assumeClass Class {classx} = classx
assumeClass _ = error "not class"

instance Shift TypeDeclaration where
  shift = shiftDefault

instance Shift.Functor TypeDeclaration where
  map category = \case
    Data {name, datax} ->
      Data
        { name,
          datax = Shift.map category datax
        }
    Class {name, classx} ->
      Class
        { name,
          classx = Shift.map category classx
        }
    Synonym {name, definition} ->
      Synonym
        { name,
          definition = Shift.map (Shift.Over category) definition
        }

instance Term.Functor TypeDeclaration where
  map Term.Category {general} = Shift.map general

simplify :: Solved.TypeDeclaration scope -> TypeDeclaration scope
simplify = \case
  Solved.ADT {name, parameters, constructors, selectors} ->
    Data
      { name,
        datax =
          Data.Data
            { parameters,
              constructors = Constructor.simplify <$> constructors,
              selectors
            }
      }
  Solved.Class {name, parameter, constraints, methods} ->
    Class
      { name,
        classx =
          Class.Class
            { parameter,
              constraints = Constraint.simplify <$> constraints,
              methods = Solved.Method.annotation' <$> methods
            }
      }
  Solved.Synonym {name, definition' = definition} -> Synonym {name, definition}
