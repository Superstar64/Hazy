module Semantic.Check.Derive.Enum where

import {-# SOURCE #-} qualified Builtin.Enum as Enum
import Control.Monad.ST (ST)
import Core.Temporary.Definition (Definition (..))
import qualified Core.Temporary.Definition as Definition
import Core.Temporary.Function (Function (..))
import Core.Temporary.Pattern (Bindings (..), Pattern (..))
import qualified Core.Temporary.Pattern as Pattern
import Core.Temporary.RightHandSide (RightHandSide (..))
import qualified Core.Tree.Class as Class
import Core.Tree.Combinators.Delay (Delay (..))
import qualified Core.Tree.Constructor as Constructor (ConstructorF (..))
import Core.Tree.Data (Data, Types (..))
import qualified Core.Tree.Data as Data
import Core.Tree.Evidence (Evidence)
import qualified Core.Tree.Evidence as Evidence
import Core.Tree.Expression (Expression)
import qualified Core.Tree.Expression as Expression
import qualified Core.Tree.Instanciation as Instanciation
import qualified Core.Tree.Type as Core
import qualified Core.Tree.TypeLambda as TypeLambda
import Data.Foldable (toList)
import qualified Data.Vector.Strict as Strict
import qualified Data.Vector.Strict as Strict.Vector
import Error (derivingNonEnum)
import Semantic.Check.ConstructorInstance (ConstructorInstance (..))
import qualified Semantic.Check.ConstructorInstance as ConstructorInstance
import Semantic.Check.Context (Context)
import Semantic.Check.Simple.Data (instanciateMark)
import qualified Semantic.Check.Temporary.ConstructorInfo as ConstructorInfo
import qualified Semantic.Index.Constructor as Constructor (Index (..))
import qualified Semantic.Index.Evidence as Index.Evidence
import qualified Semantic.Index.Local as Local
import qualified Semantic.Index.Method as Method
import qualified Semantic.Index.Type2 as Type2
import Semantic.Scope (Environment (..), Local)
import Semantic.Shift (shift)
import Semantic.Stage (Check)
import Semantic.Tree.Combinators.Inferred (Inferred (Solved))
import Semantic.Tree.MethodConcrete (Auto, MethodConcrete (Generated))
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)
import Prelude hiding (Eq)

fromEnum' ::
  Type2.Index scope ->
  Strict.Vector (ConstructorInstance s scope) ->
  Unify.Solve s (Expression scope)
fromEnum' typeIndex constructors = do
  let toIntN index constructor@Constructor.Constructor {entries} =
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
                              { typeIndex,
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
  declarations <- sequence $ zipWith toIntN [0 ..] (toList constructors)
  pure $ Definition.desugar $ foldr1 (<>) declarations

toEnum' ::
  Position ->
  Type2.Index scope ->
  Strict.Vector (ConstructorInstance s scope) ->
  ST s (Unify.Solve s (Expression scope))
toEnum' _ typeIndex constructors
  | let singleton Constructor.Constructor {entries} = null entries,
    all singleton constructors = do
      pure $ do
        let fromInt index constructor =
              do
                info <- ConstructorInfo.solve $ ConstructorInstance.info constructor
                let patternx =
                      Match
                        { irrefutable = False,
                          match =
                            Pattern.Integer
                              { integer = fromIntegral index,
                                evidence =
                                  Evidence.Variable
                                    { variable = Index.Evidence.Direct Type2.Num Type2.Int,
                                      instanciation = Instanciation.Mono
                                    },
                                equal =
                                  Evidence.Variable
                                    { variable = Index.Evidence.Direct Type2.Eq Type2.Int,
                                      instanciation = Instanciation.Mono
                                    }
                              }
                        }
                    done =
                      shift $
                        Expression.simplifyConstructorExact
                          Constructor.Index
                            { typeIndex,
                              constructorIndex = index
                            }
                          info
                          Strict.Vector.empty
                pure
                  Definition
                    { definition =
                        Bound
                          { patternx,
                            body = Plain {plain = Done {done}}
                          }
                    }
        declarations <- sequence $ zipWith fromInt [0 ..] (toList constructors)
        pure $ Definition.desugar $ foldr1 (<>) declarations
toEnum' position _ _ = derivingNonEnum position

fromEnum,
  toEnum ::
    Context s (Local ':+ scope) ->
    Position ->
    Type2.Index scope ->
    Data scope ->
    ST s (Unify.Solve s (MethodConcrete Auto layout Check scope))
fromEnum context position typeIndex datax = do
  Data.Definition {types, constructors} <- instanciateMark context position (shift datax)
  let unify index typex = Unify.unify context position typex (Core.Variable $ Local.Local index)
  sequence $ zipWith unify [0 ..] $ case types of
    Delay (Types types) -> toList types
  pure $ do
    body <- do
      fromEnum <- fromEnum' (shift typeIndex) constructors
      pure fromEnum
    pure $ Generated $ Solved $ TypeLambda.mono body
toEnum context position typeIndex datax = do
  Data.Definition {types, constructors} <- instanciateMark context position (shift datax)
  let unify index typex = Unify.unify context position typex (Core.Variable $ Local.Local index)
  sequence $ zipWith unify [0 ..] $ case types of
    Delay (Types types) -> toList types
  body <- toEnum' position (shift typeIndex) constructors
  pure $ do
    body <- body
    pure $ Generated $ Solved $ TypeLambda.mono $ body

enumFromThen ::
  Context s (Local ':+ scope) ->
  Position ->
  Evidence (Local ':+ scope) ->
  Type2.Index scope ->
  Data scope ->
  ST s (Unify.Solve s (MethodConcrete Auto layout Check scope))
enumFromThen context position evidence typeIndex datax = do
  Data.Definition {types, constructors} <- instanciateMark context position (shift datax)
  let unify index typex = Unify.unify context position typex (Core.Variable $ Local.Local index)
  sequence $ zipWith unify [0 ..] $ case types of
    Delay (Types types) -> toList types
  pure $ do
    minInfo <- ConstructorInfo.solve $ ConstructorInstance.info (Strict.Vector.head constructors)
    maxInfo <- ConstructorInfo.solve $ ConstructorInstance.info (Strict.Vector.last constructors)
    fromEnum <- fromEnum' (shift typeIndex) constructors
    let max =
          shift $
            shift $
              Expression.simplifyConstructorExact
                Constructor.Index
                  { typeIndex = shift typeIndex,
                    constructorIndex = length constructors - 1
                  }
                maxInfo
                Strict.Vector.empty
        min =
          shift $
            shift $
              Expression.simplifyConstructorExact
                Constructor.Index
                  { typeIndex = shift typeIndex,
                    constructorIndex = 0
                  }
                minInfo
                Strict.Vector.empty
        condition =
          Expression.lessThenEqualInt
            (shift (shift fromEnum) `Expression.call` shift Expression.lambdaVariable)
            (shift (shift fromEnum) `Expression.call` Expression.lambdaVariable)
        body =
          Expression.Lambda
            { body =
                Expression.Lambda
                  { body =
                      Expression.Method
                        { method = Method.enumFromThenTo,
                          evidence = shift $ shift evidence,
                          instanciation = Instanciation.Mono,
                          methodInfo = Class.info $ Enum.definition
                        }
                        `Expression.call` shift Expression.lambdaVariable
                        `Expression.call` Expression.lambdaVariable
                        `Expression.call` Expression.ifx condition max min
                  }
            }
    pure $ Generated $ Solved $ TypeLambda.mono body
