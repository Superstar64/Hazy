module Semantic.Check.Derive.Enum where

import Control.Monad.ST (ST)
import Core.Temporary.Definition (Definition (..))
import qualified Core.Temporary.Definition as Definition
import Core.Temporary.Function (Function (..))
import Core.Temporary.Pattern (Bindings (..), Pattern (..))
import qualified Core.Temporary.Pattern as Pattern
import Core.Temporary.RightHandSide (RightHandSide (..))
import Core.Tree.Data (Data)
import qualified Core.Tree.Evidence as Evidence
import Core.Tree.Expression (Expression)
import qualified Core.Tree.Expression as Expression
import qualified Core.Tree.Instanciation as Instanciation
import qualified Core.Tree.Statements as Statements
import qualified Core.Tree.Type as Core
import qualified Core.Tree.TypeLambda as TypeLambda
import Data.Foldable (toList)
import qualified Data.Vector.Strict as Strict
import qualified Data.Vector.Strict as Strict.Vector
import Error (derivingNonEnum)
import Semantic.Check.ConstructorInstance (ConstructorInstance (..))
import qualified Semantic.Check.ConstructorInstance as ConstructorInstance
import Semantic.Check.Context (Context)
import Semantic.Check.DataInstance (DataInstance (..))
import Semantic.Check.Simple.Data (instanciate)
import qualified Semantic.Check.Temporary.ConstructorInfo as ConstructorInfo
import qualified Semantic.Index.Constructor as Constructor
import qualified Semantic.Index.Evidence as Index.Evidence
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

fromEnum' ::
  Type2.Index scope ->
  Strict.Vector (ConstructorInstance s scope) ->
  Unify.Solve s (Expression scope)
fromEnum' typeIndex constructors = do
  let toIntN index constructor@ConstructorInstance {entries} =
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
  | let singleton ConstructorInstance {entries} = null entries,
    all singleton constructors = do
      pure $ do
        let fromInt index constructor@ConstructorInstance {} =
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
  toEnum,
  enumFromThen ::
    Context s (Local ':+ scope) ->
    Position ->
    Type2.Index scope ->
    Data scope ->
    ST s (Unify.Solve s (MethodConcrete Auto layout Check scope))
fromEnum context position typeIndex datax = do
  DataInstance {types, constructors} <- instanciate context position (shift datax)
  let unify index typex = Unify.unify context position typex (Core.Variable $ Local.Local index)
  sequence $ zipWith unify [0 ..] (toList types)
  pure $ do
    body <- do
      fromEnum <- fromEnum' (shift typeIndex) constructors
      pure fromEnum
    pure $ Generated $ Solved $ TypeLambda.mono body
toEnum context position typeIndex datax = do
  DataInstance {types, constructors} <- instanciate context position (shift datax)
  let unify index typex = Unify.unify context position typex (Core.Variable $ Local.Local index)
  sequence $ zipWith unify [0 ..] (toList types)
  body <- toEnum' position (shift typeIndex) constructors
  pure $ do
    body <- body
    pure $ Generated $ Solved $ TypeLambda.mono $ body
enumFromThen context position _ datax = do
  DataInstance {types} <- instanciate context position (shift datax)
  let unify index typex = Unify.unify context position typex (Core.Variable $ Local.Local index)
  sequence $ zipWith unify [0 ..] (toList types)
  pure $ do
    let body =
          Expression.Join {statements = Statements.Bottom}
    pure $ Generated $ Solved $ TypeLambda.mono body
