module Semantic.Check.Derive.Ord where

import Control.Monad.ST (ST)
import Core.Temporary.Definition (Definition (..))
import qualified Core.Temporary.Definition as Definition
import Core.Temporary.Function (Function (..))
import Core.Temporary.Pattern (Bindings (..), Pattern (..))
import qualified Core.Temporary.Pattern as Pattern
import Core.Temporary.RightHandSide (RightHandSide (..))
import Core.Tree.Data (Data)
import qualified Core.Tree.Expression as Expression
import qualified Core.Tree.Statements as Statements
import qualified Core.Tree.Type as Core
import qualified Core.Tree.TypeLambda as TypeLambda
import Data.Foldable (toList)
import qualified Data.Vector.Strict as Strict
import qualified Data.Vector.Strict as Strict.Vector
import Semantic.Check.ConstructorInstance (ConstructorInstance (..))
import qualified Semantic.Check.ConstructorInstance as ConstructorInstance
import Semantic.Check.Context (Context)
import Semantic.Check.DataInstance (DataInstance (..))
import Semantic.Check.EntryInstance (entry)
import Semantic.Check.Simple.Data (instanciate)
import qualified Semantic.Check.Temporary.ConstructorInfo as ConstructorInfo
import qualified Semantic.Index.Constructor as Constructor
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

compare ::
  Context s (Local ':+ scope) ->
  Position ->
  Type2.Index scope ->
  Data scope ->
  ST s (Unify.Solve s (MethodConcrete Auto layout Check scope))
compare context position typeIndex datax = do
  DataInstance {types, constructors} <- instanciate context position (shift datax)
  let unify index typex = Unify.unify context position typex (Core.Variable $ Local.Local index)
  sequence $ zipWith unify [0 ..] (toList types)
  let generate index constructor@ConstructorInstance {entries} = do
        let patterns :: Strict.Vector (Pattern scope)
            patterns = Strict.Vector.replicate (length entries) Wildcard
            types = entry <$> entries
        evidence <- traverse (Unify.constrain context position Type2.Ord) types
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
                Expression.compare
                  (shift $ shift evidence)
                  (shift $ Expression.patternVariableAt' index)
                  (Expression.patternVariableAt' index)
              done =
                foldr
                  (Expression.orderingCombine)
                  Expression.orderingEqual
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
      toIntN index constructor@ConstructorInstance {entries} =
        do
          info <- ConstructorInfo.solve $ ConstructorInstance.info constructor
          let patterns :: Strict.Vector (Pattern scope)
              patterns = Strict.Vector.replicate (length entries) Wildcard
              patternx =
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
              done = Expression.int index
          pure
            Definition
              { definition =
                  Bound
                    { patternx,
                      body = Plain {plain = Done {done}}
                    }
              }
  definitions <- sequence $ zipWith generate [0 ..] (toList constructors)
  let otherwise = do
        toInts <- sequence $ zipWith toIntN [0 ..] (toList constructors)
        let toInt = shift $ shift $ Definition.desugar $ foldr1 (<>) toInts
            done
              | null toInts = Expression.Join {statements = Statements.Bottom}
              | True =
                  Expression.compareInt
                    (Expression.call toInt $ shift Expression.patternVariable)
                    (Expression.call toInt Expression.patternVariable)
        pure
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
  pure $ do
    definitions <- sequence definitions
    otherwise <- otherwise
    pure $
      Generated $
        Solved $
          TypeLambda.mono $
            if null definitions
              then Expression.orderingEqual
              else Definition.desugar $ foldr (<>) otherwise definitions
