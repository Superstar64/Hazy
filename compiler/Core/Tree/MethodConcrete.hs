module Core.Tree.MethodConcrete where

import qualified Core.Substitute as Substitute
import qualified Core.Tree.Expression as Expression
import Core.Tree.TypeLambda (TypeLambda)
import qualified Core.Tree.TypeLambda as TypeLambda
import Semantic.Layout (Normal)
import Semantic.Scope (Environment (..), Local)
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Semantic.Stage (Check)
import Semantic.Tree.Combinators.Implicit (Implicit (..))
import Semantic.Tree.Combinators.Inferred (Inferred (Solved))
import qualified Semantic.Tree.MethodConcrete as Semantic

newtype MethodConcrete scope = Definition
  { definition :: TypeLambda (Local ':+ scope)
  }
  deriving (Show)

instance Shift0.Functor MethodConcrete where
  map = Shift.mapDefault

instance Shift.Functor MethodConcrete where
  map = Substitute.mapDefault

instance Substitute.Functor MethodConcrete where
  map category Definition {definition} =
    Definition
      { definition = Substitute.map (Substitute.Over category) definition
      }

simplify :: Semantic.MethodConcrete origin Normal Check scope -> MethodConcrete scope
simplify = \case
  Semantic.Definition (Check definition) ->
    Definition
      { definition = TypeLambda.map (TypeLambda.Map Expression.simplify) definition
      }
  Semantic.Generated (Solved automatic) -> Definition {definition = automatic}
