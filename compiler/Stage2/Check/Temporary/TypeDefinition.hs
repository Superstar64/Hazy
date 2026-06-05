module Stage2.Check.Temporary.TypeDefinition where

import Control.Monad.ST (ST)
import qualified Data.Vector.Strict as Strict
import qualified Data.Vector.Strict as Strict.Vector
import Error (unsupportedFeatureGADTs)
import Stage1.Position (Position)
import Stage1.Tree.Brand (Brand)
import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope
import Stage2.Stage (Check, Resolve)
import Stage2.Tree.Selector (Selector)
import Stage2.Tree.TypeDefinition (Constructive, Inject (..))
import qualified Stage2.Tree.TypeDefinition as Solved
import qualified Stage2.Tree.TypeDefinition as Stage2 (TypeDefinition (..))
import qualified Stage2.Tree.TypePattern as Stage2 (TypePattern (..))
import Stage2.Check.Context (Context)
import Stage2.Check.Temporary.Constraint (Constraint)
import qualified Stage2.Check.Temporary.Constraint as Constraint
import Stage2.Check.Temporary.Constructor (Constructor)
import qualified Stage2.Check.Temporary.Constructor as Constructor
import Stage2.Check.Temporary.Method (Method)
import qualified Stage2.Check.Temporary.Method as Method
import qualified Stage2.Check.Temporary.Scheme as Scheme
import Stage2.Check.Temporary.TypePattern (TypePattern (..))
import qualified Stage2.Check.Temporary.TypePattern as TypePattern
import qualified Stage2.Unify as Unify

data TypeDefinition s scope
  = ADT
      { brand :: !Brand,
        position :: !Position,
        parameters :: !(Strict.Vector (TypePattern s scope)),
        constructors :: !(Strict.Vector (Constructor s (Scope.Local ':+ scope))),
        selectors :: !(Strict.Vector Selector)
      }
  | Class
      { position :: !Position,
        parameter :: !(TypePattern s scope),
        constraints :: !(Strict.Vector (Constraint s scope)),
        methods :: !(Strict.Vector (Method s (Scope.Local ':+ scope)))
      }

check ::
  Context s scope ->
  Unify.Type s scope ->
  Stage2.TypeDefinition Constructive Resolve scope ->
  ST s (TypeDefinition s scope)
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
              position,
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
          { position,
            parameter,
            constraints,
            methods
          }
  Stage2.GADT {position} -> unsupportedFeatureGADTs position
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

solve :: Context s scope -> TypeDefinition s scope -> Unify.Solve s (Solved.TypeDefinition Constructive Check scope)
solve context = \case
  ADT {brand, position, parameters, constructors, selectors} -> do
    parameters <- traverse TypePattern.solve parameters
    context <- pure $ Scheme.augmentSolve parameters context
    constructors <- traverse (Constructor.solve context) constructors
    pure Solved.ADT {position, brand, parameters, constructors, selectors, inject = Inject}
  Class {parameter, position, constraints, methods} -> do
    parameter <- TypePattern.solve parameter
    context <- pure $ Scheme.augmentSolve (Strict.Vector.singleton parameter) context
    constraints <- traverse (Constraint.solve context) constraints
    methods <- traverse (Method.solve context) methods
    pure Solved.Class {position, parameter, constraints, methods, inject = Inject}
