module Semantic.Check.Temporary.TypeDefinition where

import Control.Monad.ST (ST)
import qualified Data.Vector.Strict as Strict
import qualified Data.Vector.Strict as Strict.Vector
import Error (unsupportedFeatureGADTs)
import Semantic.Check.Context (Context)
import Semantic.Check.Temporary.Constraint (Constraint)
import qualified Semantic.Check.Temporary.Constraint as Constraint
import Semantic.Check.Temporary.Constructor (Constructor)
import qualified Semantic.Check.Temporary.Constructor as Constructor
import Semantic.Check.Temporary.Method (Method)
import qualified Semantic.Check.Temporary.Method as Method
import qualified Semantic.Check.Temporary.Scheme as Scheme
import Semantic.Check.Temporary.TypePattern (TypePattern (..))
import qualified Semantic.Check.Temporary.TypePattern as TypePattern
import Semantic.Scope (Environment (..))
import qualified Semantic.Scope as Scope
import Semantic.Stage (Check, Resolve)
import Semantic.Tree.Selector (Selector)
import Semantic.Tree.TypeDefinition (Constructive, Inject (..))
import qualified Semantic.Tree.TypeDefinition as Semantic (TypeDefinition (..))
import qualified Semantic.Tree.TypeDefinition as Solved
import qualified Semantic.Tree.TypePattern as Semantic (TypePattern (..))
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)
import Syntax.Tree.Brand (Brand)

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
  Semantic.TypeDefinition Constructive Resolve scope ->
  ST s (TypeDefinition s scope)
check context kind = \case
  Semantic.ADT
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
  Semantic.Class
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
  Semantic.GADT {position} -> unsupportedFeatureGADTs position
  where
    fresh Semantic.TypePattern {name, position} = do
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
