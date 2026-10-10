module Semantic.Check.Derive.Eq where

import Control.Monad.ST (ST)
import Core.Temporary.Definition (Definition (..))
import qualified Core.Temporary.Definition as Definition
import Core.Temporary.Function (Function (..))
import Core.Temporary.Pattern (Bindings (..), Pattern (..))
import qualified Core.Temporary.Pattern as Pattern
import Core.Temporary.RightHandSide (RightHandSide (..))
import Core.Tree.Combinators.Delay (Delay (..))
import qualified Core.Tree.Constructor as Constructor (ConstructorF (..))
import Core.Tree.Data (Data, Types (..))
import qualified Core.Tree.Data as Data
import Core.Tree.Entry (entry)
import qualified Core.Tree.Expression as Expression
import qualified Core.Tree.Type as Core
import qualified Core.Tree.TypeLambda as TypeLambda
import Data.Foldable (toList)
import qualified Data.Vector.Strict as Strict
import qualified Data.Vector.Strict as Strict.Vector
import qualified Semantic.Check.ConstructorInstance as ConstructorInstance
import Semantic.Check.Context (Context)
import Semantic.Check.Simple.Data (instanciateMark)
import qualified Semantic.Check.Temporary.ConstructorInfo as ConstructorInfo
import qualified Semantic.Index.Constructor as Constructor (Index (..))
import qualified Semantic.Index.Local as Local
import qualified Semantic.Index.Type2 as Type2
import Semantic.Scope (Environment (..), Local)
import Semantic.Shift (shift)
import Semantic.Stage (Check)
import Semantic.Tree.Combinators.Inferred (Inferred (Solved))
import Semantic.Tree.MethodConcrete (Auto, MethodConcrete (Generated))
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)
import Prelude hiding (Eq)

equal ::
  Context s (Local ':+ scope) ->
  Position ->
  Type2.Index scope ->
  Data scope ->
  ST s (Unify.Solve s (MethodConcrete Auto layout Check scope))
equal context position typeIndex datax = do
  Data.Definition {types, constructors} <- instanciateMark context position (shift datax)
  let unify index typex = Unify.unify context position typex (Core.Variable $ Local.Local index)
  sequence $ zipWith unify [0 ..] $ case types of
    Delay (Types types) -> toList types
  let generate index constructor@Constructor.Constructor {entries} = do
        let patterns :: Strict.Vector (Pattern scope)
            patterns = Strict.Vector.replicate (length entries) Wildcard
            types = entry <$> entries
        evidence <- traverse (Unify.constrain context position Type2.Eq) types
        pure $ do
          evidence <- traverse (Unify.solveEvidence position) evidence
          info <- ConstructorInfo.solve $ ConstructorInstance.info constructor
          let patternx =
                Match
                  { irrefutable = False,
                    match =
                      Constructor
                        { constructor =
                            Constructor.Index
                              { typeIndex = shift typeIndex,
                                constructorIndex = index
                              },
                          patterns,
                          constructorInfo = info
                        }
                  }
              compare index evidence =
                Expression.eq
                  (shift $ shift evidence)
                  (shift $ Expression.patternVariableAt' index)
                  (Expression.patternVariableAt' index)
              done =
                foldr
                  (Expression.&&)
                  Expression.true
                  (zipWith compare [0 ..] (toList evidence))
          pure
            Definition
              { definition =
                  Bound
                    { patternx,
                      body =
                        Bound
                          { patternx = shift patternx,
                            body = Plain {plain = Done {done}}
                          }
                    }
              }
  definitions <- sequence $ zipWith generate [0 ..] (toList constructors)
  pure $ do
    definitions <- sequence definitions
    body <-
      if null definitions
        then
          pure $ Expression.Lambda $ Expression.Lambda $ Expression.true
        else do
          let done = Expression.false
              otherwise =
                Definition
                  { definition =
                      Bound
                        { patternx = Pattern.Wildcard,
                          body =
                            Bound
                              { patternx = Pattern.Wildcard,
                                body = Plain {plain = Done {done}}
                              }
                        }
                  }
          pure $ Definition.desugar $ foldr (<>) otherwise definitions
    pure $ Generated $ Solved $ TypeLambda.mono body
