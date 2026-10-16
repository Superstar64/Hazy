module Semantic.Check.Derive.Ord where

import Control.Monad.ST (ST)
import qualified Core.Tree.Constructor as Constructor (ConstructorF (..))
import qualified Core.Tree.ConstructorInfo as Core
import Core.Tree.Data (Data)
import qualified Core.Tree.Data as Data
import Core.Tree.Entry (entry)
import Core.Tree.EntryInfo (EntryInfo (..))
import Core.Tree.Expression (Direction (..), lambdaVariable)
import qualified Core.Tree.Expression as Expression
import qualified Core.Tree.Statements as Statements
import qualified Core.Tree.Type as Type
import qualified Core.Tree.TypeLambda as TypeLambda
import Data.Foldable (toList)
import qualified Data.Vector.Strict as Strict.Vector
import Semantic.Check.Context (Context)
import Semantic.Check.Derive.Enum (fromEnum')
import Semantic.Check.Info.Constructor (ConstructorInfo (..))
import qualified Semantic.Check.Instanciate.Constructor as ConstructorInstance
import Semantic.Check.Instanciate.Data (instanciateRigid)
import qualified Semantic.Check.Temporary.ConstructorInfo as ConstructorInfo
import qualified Semantic.Index.Constructor as Constructor (Index (..))
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
  Data.Definition {constructors} <- instanciateRigid context position datax
  let generate constructorIndex constructor@Constructor.Constructor {entries} = do
        let types = entry <$> entries
        evidence <- traverse (Unify.constrain context position Type2.Ord) types
        pure $ do
          evidence <- traverse (Unify.solveEvidence position) evidence
          info <- ConstructorInfo.solve $ ConstructorInstance.info constructor
          let index =
                Constructor.Index
                  { typeIndex,
                    constructorIndex
                  }
          pure $ case info of
            ConstructorInfo {entries}
              | info <- Core.ConstructorInfo {entries} ->
                  Statements.Bind
                    { constructor = shift $ shift $ shift index,
                      constructorInfo = shift $ shift $ info,
                      check = shift $ lambdaVariable,
                      thenx =
                        Statements.Bind
                          { constructor = shift $ shift $ shift $ shift index,
                            constructorInfo = shift $ shift $ shift info,
                            check = shift $ lambdaVariable,
                            thenx =
                              Statements.Done
                                { done =
                                    let compare index evidence
                                          | EntryInfo {levity} <- entries Strict.Vector.! index,
                                            levity <- shift $ shift levity =
                                              Expression.compare
                                                (shift $ shift $ shift $ shift evidence)
                                                (shift $ Expression.patternVariableAt levity index)
                                                (Expression.patternVariableAt (shift levity) index)
                                     in foldr
                                          Expression.orderingCombine
                                          Expression.orderingEqual
                                          (zipWith compare [0 ..] (toList evidence))
                                }
                          }
                    }
            Newtype ->
              Statements.Done
                { done =
                    Expression.compare
                      (shift $ shift $ Strict.Vector.head evidence)
                      Expression.Newtype
                        { constructor = shift $ shift $ shift index,
                          argument = shift $ lambdaVariable,
                          direction = Destruct
                        }
                      Expression.Newtype
                        { constructor = shift $ shift $ shift $ index,
                          argument = lambdaVariable,
                          direction = Destruct
                        }
                }
  definitions <- sequence $ zipWith generate [0 ..] (toList constructors)
  pure $ do
    definitions <- sequence definitions
    fromEnum <- shift . shift <$> fromEnum' (shift typeIndex) constructors
    body <-
      pure $
        Expression.Lambda $
          Expression.Lambda $
            if null definitions
              then Expression.orderingEqual
              else
                Expression.Join
                  { statements =
                      let branch left right = Statements.Branch {left, right}
                          done =
                            Statements.Done
                              { done =
                                  Expression.compareInt
                                    (Expression.call fromEnum $ shift Expression.lambdaVariable)
                                    (Expression.call fromEnum Expression.lambdaVariable)
                              }
                       in foldr branch done definitions
                  }
    pure $ Generated $ Solved $ TypeLambda.mono Type.lazy body
