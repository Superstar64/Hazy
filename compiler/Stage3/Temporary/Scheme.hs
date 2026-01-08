{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage3.Temporary.Scheme where

import Control.Monad.ST (ST)
import Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import Stage1.Position (Position)
import qualified Stage2.Index.Table.Local as Local
import qualified Stage2.Index.Table.Term as Term
import qualified Stage2.Index.Table.Type as Type (Table (..))
import qualified Stage2.Label.Binding.Local as Label
import Stage2.Scope (Environment (..), Local)
import Stage2.Shift (Shift (..))
import qualified Stage2.Tree.Scheme as Stage2 (Scheme (..))
import qualified Stage2.Tree.TypePattern as Stage2 (TypePattern (..))
import Stage3.Check.Context (Context (..))
import Stage3.Check.LocalBinding (LocalBinding (Wobbly, label, wobbly))
import qualified Stage3.Synonym as Synonym
import Stage3.Temporary.Constraint (Constraint)
import qualified Stage3.Temporary.Constraint as Constraint
import Stage3.Temporary.Type (Type)
import {-# SOURCE #-} qualified Stage3.Temporary.Type as Type (check, solve)
import Stage3.Temporary.TypePattern (TypePattern (TypePattern))
import qualified Stage3.Temporary.TypePattern as TypePattern
import qualified Stage3.Tree.Scheme as Solved
import qualified Stage3.Unify as Unify

data Scheme s scope = Scheme
  { parameters :: !(Strict.Vector (TypePattern s scope)),
    constraints :: !(Strict.Vector (Constraint s scope)),
    result :: !(Type s (Local ':+ scope))
  }

check :: Context s scope -> Stage2.Scheme Position scope -> ST s (Scheme s scope)
check context (Stage2.Scheme {parameters, constraints, result}) = do
  let fresh Stage2.TypePattern {position, name} = do
        level <- Unify.fresh Unify.universe
        typex <- Unify.fresh (Unify.typeWith level)
        pure TypePattern {position, name, typex}
  parameters <- traverse fresh parameters
  constraints <- traverse (Constraint.check (augment parameters context)) constraints
  result <- Type.check (augment parameters context) Unify.typex result
  pure $ Scheme {parameters, constraints, result}

solve :: Synonym.Context s scope -> Scheme s scope -> ST s (Solved.Scheme scope)
solve context Scheme {parameters, constraints, result} = do
  parameters <- traverse TypePattern.solve parameters
  constraints <- traverse (Constraint.solve (Synonym.local context)) constraints
  result <- Type.solve (Synonym.local context) result
  pure $ Solved.Scheme {parameters, constraints, result}

augment :: Strict.Vector (TypePattern s scope) -> Context s scope -> Context s (Local ':+ scope)
augment scheme Context {termEnvironment, localEnvironment, typeEnvironment}
  | scheme <- Strict.Vector.toLazy scheme =
      Context
        { termEnvironment = Term.Local termEnvironment,
          localEnvironment = Local.Local (fmap wobbly scheme) localEnvironment,
          typeEnvironment = Type.Local typeEnvironment
        }
  where
    wobbly
      TypePattern {name, typex = wobbly}
        | wobbly <- shift wobbly =
            Wobbly {label = Label.LocalBinding {name}, wobbly}
