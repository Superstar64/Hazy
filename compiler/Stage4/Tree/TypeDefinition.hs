module Stage4.Tree.TypeDefinition where

import Stage2.Scope (Environment ((:+)), Local)
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Stage (Check)
import Stage2.Tree.Combinators.Inferred (Inferred (..))
import qualified Stage2.Tree.Method as Solved.Method
import qualified Stage2.Tree.TypeDefinition
import qualified Stage2.Tree.TypeDefinition as Solved (TypeDefinition (..))
import Stage2.Tree.TypePattern (TypePattern (..), typex')
import qualified Stage4.Shift as Shift2
import qualified Stage4.Substitute as Substitute
import Stage4.Tree.Class (Class)
import qualified Stage4.Tree.Class as Class
import qualified Stage4.Tree.Constraint as Constraint
import qualified Stage4.Tree.Constructor as Constructor
import Stage4.Tree.Data (Data)
import qualified Stage4.Tree.Data as Data
import qualified Stage4.Tree.Scheme as Scheme
import Stage4.Tree.Type (Type)
import qualified Stage4.Tree.Type as Type

data TypeDefinition scope
  = Data !(Data scope)
  | Class !(Class scope)
  | Synonym !(Type (Local ':+ scope))
  deriving (Show)

assumeData :: TypeDefinition scope -> Data scope
assumeData (Data datax) = datax
assumeData _ = error "not data"

assumeClass :: TypeDefinition scope -> Class scope
assumeClass (Class classx) = classx
assumeClass _ = error "not class"

instance Shift TypeDefinition where
  shift = shiftDefault

instance Shift.Functor TypeDefinition where
  map = Shift2.mapDefault

instance Shift2.Functor TypeDefinition where
  map = Substitute.mapDefault

instance Substitute.Functor TypeDefinition where
  map category = \case
    Data datax ->
      Data (Substitute.map category datax)
    Class classx ->
      Class (Substitute.map category classx)
    Synonym definition ->
      Synonym (Substitute.map (Substitute.Over category) definition)

simplify :: Solved.TypeDefinition equality Check scope -> TypeDefinition scope
simplify = \case
  Solved.ADT {parameters, constructors, selectors, brand} ->
    Data
      Data.Data
        { parameters = fmap typex' parameters,
          constructors = Constructor.simplify <$> constructors,
          selectors,
          brand
        }
  Solved.Class {parameter = TypePattern {typex = Solved parameter}, constraints, methods} ->
    Class
      Class.Class
        { parameter,
          constraints = Constraint.simplify <$> constraints,
          methods = Scheme.simplify . Solved.Method.annotation <$> methods
        }
  Solved.Synonym {synonym} -> Synonym (Type.simplify synonym)
