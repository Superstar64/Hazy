module Core.Tree.TypeDefinition where

import qualified Core.Shift as Shift2
import qualified Core.Substitute as Substitute
import Core.Tree.Class (Class)
import qualified Core.Tree.Class as Class
import qualified Core.Tree.Constraint as Constraint
import qualified Core.Tree.Constructor as Constructor
import Core.Tree.Data (Data)
import qualified Core.Tree.Data as Data
import qualified Core.Tree.Scheme as Scheme
import Core.Tree.Type (Type)
import qualified Core.Tree.Type as Type
import Semantic.Scope (Environment ((:+)), Local)
import Semantic.Shift (Shift, shiftDefault)
import qualified Semantic.Shift as Shift
import Semantic.Stage (Check)
import Semantic.Tree.Combinators.Inferred (Inferred (..))
import qualified Semantic.Tree.Method as Solved.Method
import qualified Semantic.Tree.TypeDefinition
import qualified Semantic.Tree.TypeDefinition as Solved (TypeDefinition (..))
import Semantic.Tree.TypePattern (TypePattern (..), typex')

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
