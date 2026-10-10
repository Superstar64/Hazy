module Semantic.Check.Derive.Bounded where

import Control.Monad.ST (ST)
import qualified Core.Tree.Constructor as Constructor (ConstructorF (..))
import Core.Tree.Data (Data)
import qualified Core.Tree.Data as Data
import Core.Tree.Entry (entry)
import qualified Core.Tree.Expression as Expression
import qualified Core.Tree.TypeLambda as TypeLambda
import Data.Foldable (toList)
import qualified Data.Vector.Strict as Strict.Vector
import Error (derivingNonEnum)
import qualified Semantic.Check.ConstructorInstance as ConstructorInstance
import Semantic.Check.Context (Context)
import Semantic.Check.DataInstance (instanciateRigid)
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

data Bound = Min | Max

bound ::
  Bound ->
  Context s (Local ':+ scope) ->
  Position ->
  Type2.Index scope ->
  Data scope ->
  ST s (Unify.Solve s (MethodConcrete Auto layout Check scope))
bound bound context position typeIndex datax = do
  Data.Definition {constructors} <- instanciateRigid context position datax
  body <-
    if
      | let singleton Constructor.Constructor {entries} = null entries,
        all singleton constructors -> do
          let constructorIndex = case bound of
                Min -> 0
                Max -> length constructors - 1
              constructor = constructors Strict.Vector.! constructorIndex
          pure $ do
            info <- ConstructorInfo.solve $ ConstructorInstance.info constructor
            pure $
              Expression.simplifyConstructorExact
                Constructor.Index
                  { typeIndex = shift typeIndex,
                    constructorIndex
                  }
                info
                Strict.Vector.empty
      | [constructor@Constructor.Constructor {entries}] <- toList constructors -> do
          evidences <- traverse (Unify.constrain context position Type2.Bounded . entry) entries
          pure $ do
            info <- ConstructorInfo.solve $ ConstructorInstance.info constructor
            evidences <- traverse (Unify.solveEvidence position) evidences
            let argument evidence = case bound of
                  Min -> Expression.minBound evidence
                  Max -> Expression.maxBound evidence
                arguments = argument <$> evidences
            pure $
              Expression.simplifyConstructorExact
                Constructor.Index
                  { typeIndex = shift typeIndex,
                    constructorIndex = 0
                  }
                info
                arguments
      | otherwise -> derivingNonEnum position
  pure $ do
    body <- body
    pure $ Generated $ Solved $ TypeLambda.mono body

minBound ::
  Context s (Local ':+ scope) ->
  Position ->
  Type2.Index scope ->
  Data scope ->
  ST s (Unify.Solve s (MethodConcrete Auto layout Check scope))
minBound = bound Min

maxBound ::
  Context s (Local ':+ scope) ->
  Position ->
  Type2.Index scope ->
  Data scope ->
  ST s (Unify.Solve s (MethodConcrete Auto layout Check scope))
maxBound = bound Max
