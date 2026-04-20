module Stage3.Temporary.TypeDefinition where

import Control.Monad.ST (ST)
import qualified Data.Vector.Strict as Strict
import qualified Data.Vector.Strict as Strict.Vector
import Error (unsupportedFeatureGADTs)
import Stage1.Tree.Brand (Brand)
import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope
import Stage2.Tree.Selector (Selector)
import qualified Stage2.Tree.TypeDefinition as Stage2 (TypeDefinition (..))
import qualified Stage2.Tree.TypePattern as Stage2 (TypePattern (..))
import Stage3.Check.Context (Context)
import Stage3.Temporary.Constraint (Constraint)
import qualified Stage3.Temporary.Constraint as Constraint
import Stage3.Temporary.Constructor (Constructor)
import qualified Stage3.Temporary.Constructor as Constructor
import Stage3.Temporary.Method (Method)
import qualified Stage3.Temporary.Method as Method
import qualified Stage3.Temporary.Scheme as Scheme
import Stage3.Temporary.TypePattern (TypePattern (..))
import qualified Stage3.Temporary.TypePattern as TypePattern
import qualified Stage3.Tree.TypeDefinition as Solved
import qualified Stage3.Unify as Unify

data TypeDefinition s scope
  = ADT
      { brand :: !Brand,
        parameters :: !(Strict.Vector (TypePattern s scope)),
        constructors :: !(Strict.Vector (Constructor s (Scope.Local ':+ scope))),
        selectors :: !(Strict.Vector Selector)
      }
  | Class
      { parameter :: !(TypePattern s scope),
        constraints :: !(Strict.Vector (Constraint s scope)),
        methods :: !(Strict.Vector (Method s (Scope.Local ':+ scope)))
      }

check :: Context s scope -> Unify.Type s scope -> Stage2.TypeDefinition scope -> ST s (TypeDefinition s scope)
check context kind = \case
  Stage2.ADT
    { position,
      brand,
      constructors,
      selectors,
      parameters
    } ->
      do
        parameters <- traverse fresh parameters
        let kind' = foldr (Unify.function . TypePattern.typex) Unify.typex parameters
        Unify.unify context position kind' kind
        context <- pure $ Scheme.augment parameters context
        constructors <- traverse (Constructor.check context) constructors
        pure
          ADT
            { brand,
              constructors,
              parameters,
              selectors
            }
  Stage2.Class
    { position,
      methods,
      constraints,
      parameter
    } -> do
      parameter <- fresh parameter
      let kind' = Unify.function (TypePattern.typex parameter) Unify.constraint
      Unify.unify context position kind' kind
      context <- pure $ Scheme.augment (Strict.Vector.singleton parameter) context
      constraints <- traverse (Constraint.check context) constraints
      methods <- traverse (Method.check context) methods
      pure
        Class
          { parameter,
            constraints,
            methods
          }
  Stage2.GADT {position} -> unsupportedFeatureGADTs position
  Stage2.Synonym {} -> error "can't type check synonym"
  where
    fresh Stage2.TypePattern {name, position} = do
      level <- Unify.fresh Unify.universe
      typex <- Unify.fresh (Unify.typeWith level)
      pure
        TypePattern
          { name,
            typex,
            position
          }

solve :: Context s scope -> TypeDefinition s scope -> ST s (Solved.TypeDefinition scope)
solve context = \case
  ADT {brand, parameters, constructors, selectors} -> do
    parameters <- traverse TypePattern.solve parameters
    context <- pure $ Scheme.augmentSolve parameters context
    constructors <- traverse (Constructor.solve context) constructors
    pure Solved.ADT {brand, parameters, constructors, selectors}
  Class {parameter, constraints, methods} -> do
    parameter <- TypePattern.solve parameter
    context <- pure $ Scheme.augmentSolve (Strict.Vector.singleton parameter) context
    constraints <- traverse (Constraint.solve context) constraints
    methods <- traverse (Method.solve context) methods
    pure Solved.Class {parameter, constraints, methods}
