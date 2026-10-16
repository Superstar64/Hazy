module Semantic.Check.Derive.Enum where

import {-# SOURCE #-} qualified Builtin.Enum as Enum
import Control.Monad.ST (ST)
import qualified Core.Tree.Class as Class
import qualified Core.Tree.Constructor as Constructor (ConstructorF (..))
import qualified Core.Tree.ConstructorInfo as Core
import Core.Tree.Data (Data)
import qualified Core.Tree.Data as Data
import Core.Tree.Evidence (Evidence)
import qualified Core.Tree.Evidence as Evidence
import Core.Tree.Expression (Expression, lambdaVariable)
import qualified Core.Tree.Expression as Expression
import qualified Core.Tree.Instanciation as Instanciation
import qualified Core.Tree.Statements as Statements
import qualified Core.Tree.Type as Type
import qualified Core.Tree.TypeLambda as TypeLambda
import Data.Foldable (toList)
import qualified Data.Vector.Strict as Strict
import qualified Data.Vector.Strict as Strict.Vector
import Error (derivingNonEnum)
import Semantic.Check.Context (Context)
import Semantic.Check.Info.Constructor (ConstructorInfo (..))
import Semantic.Check.Instanciate.Constructor (ConstructorInstance (..))
import qualified Semantic.Check.Instanciate.Constructor as ConstructorInstance
import Semantic.Check.Instanciate.Data (instanciateRigid)
import qualified Semantic.Check.Temporary.ConstructorInfo as ConstructorInfo
import qualified Semantic.Index.Constructor as Constructor (Index (..))
import qualified Semantic.Index.Evidence as Index.Evidence
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
  let toIntN constructorIndex constructor =
        do
          info <- ConstructorInfo.solve $ ConstructorInstance.info constructor
          let index =
                Constructor.Index
                  { typeIndex,
                    constructorIndex
                  }
          case info of
            ConstructorInfo {entries}
              | info <- Core.ConstructorInfo {entries} ->
                  pure
                    Statements.Bind
                      { constructor = shift index,
                        constructorInfo = shift info,
                        check = lambdaVariable,
                        thenx =
                          Statements.Done
                            { done = Expression.int constructorIndex
                            }
                      }
            Newtype {} ->
              pure
                Statements.Done
                  { done = Expression.int 0
                  }
  declarations <- sequence $ zipWith toIntN [0 ..] (toList constructors)
  pure $
    Expression.Lambda $
      Expression.Join
        { statements =
            let branch left right = Statements.Branch {left, right}
             in foldr branch Statements.Bottom declarations
        }

toEnum' ::
  Position ->
  Type2.Index scope ->
  Strict.Vector (ConstructorInstance s scope) ->
  ST s (Unify.Solve s (Expression scope))
toEnum' _ typeIndex constructors
  | let singleton Constructor.Constructor {entries} = null entries,
    all singleton constructors = do
      pure $ do
        let fromInt constructorIndex constructor =
              do
                info <- ConstructorInfo.solve $ ConstructorInstance.info constructor
                let index =
                      Constructor.Index
                        { typeIndex,
                          constructorIndex = constructorIndex
                        }
                pure $
                  let eqInt =
                        Evidence.Variable
                          { variable = Index.Evidence.Direct Type2.Eq Type2.Int,
                            instanciation = Instanciation.monoLazy
                          }
                      check = Expression.eq eqInt lambdaVariable $ Expression.int constructorIndex
                      done = Expression.simplifyConstructorExact (shift index) (shift info) Strict.Vector.empty
                   in Statements.ifx check done
        declarations <- sequence $ zipWith fromInt [0 ..] (toList constructors)
        pure $
          Expression.Lambda $
            Expression.Join
              { statements =
                  let branch left right = Statements.Branch {left, right}
                   in foldr branch Statements.Bottom declarations
              }
toEnum' position _ _ = derivingNonEnum position

fromEnum,
  toEnum ::
    Context s (Local ':+ scope) ->
    Position ->
    Type2.Index scope ->
    Data scope ->
    ST s (Unify.Solve s (MethodConcrete Auto layout Check scope))
fromEnum context position typeIndex datax = do
  Data.Definition {constructors} <- instanciateRigid context position datax
  pure $ do
    body <- do
      fromEnum <- fromEnum' (shift typeIndex) constructors
      pure fromEnum
    pure $ Generated $ Solved $ TypeLambda.mono Type.lazy body
toEnum context position typeIndex datax = do
  Data.Definition {constructors} <- instanciateRigid context position datax
  body <- toEnum' position (shift typeIndex) constructors
  pure $ do
    body <- body
    pure $ Generated $ Solved $ TypeLambda.mono Type.lazy $ body

enumFromThen ::
  Context s (Local ':+ scope) ->
  Position ->
  Evidence (Local ':+ scope) ->
  Type2.Index scope ->
  Data scope ->
  ST s (Unify.Solve s (MethodConcrete Auto layout Check scope))
enumFromThen context position evidence typeIndex datax = do
  Data.Definition {constructors} <- instanciateRigid context position datax
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
                          instanciation = Instanciation.monoLazy,
                          methodInfo = Class.info $ Enum.definition
                        }
                        `Expression.call` shift Expression.lambdaVariable
                        `Expression.call` Expression.lambdaVariable
                        `Expression.call` Expression.ifx condition max min
                  }
            }
    pure $ Generated $ Solved $ TypeLambda.mono Type.lazy body
