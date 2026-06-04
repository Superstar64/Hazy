{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage2.Resolve.Go.Scheme where

import Data.Containers.ListUtils (nubOrd)
import Data.Foldable (toList)
import qualified Data.Map as Map
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import qualified Stage1.FreeVariables as Stage1 (FreeTypeVariables (..))
import Stage1.Position (Position)
import Stage1.Tree.Marked (Marked (..))
import qualified Stage1.Tree.Scheme as Stage1 (Scheme (..))
import qualified Stage2.Index.Local as Local
import Stage2.Resolve.Context (Context (..))
import qualified Stage2.Resolve.Go.Constraint as Constraint
import qualified Stage2.Resolve.Go.Type as Type
import qualified Stage2.Resolve.Go.TypePattern as TypePattern (resolve)
import Stage2.Scope (Environment ((:+)), Local)
import Stage2.Shift (shift)
import Stage2.Stage (Resolve)
import Stage2.Tree.Combinators.Inferred (Inferred (..))
import Stage2.Tree.Scheme (Scheme (..))
import Stage2.Tree.TypePattern (TypePattern (TypePattern))
import qualified Stage2.Tree.TypePattern as TypePattern (TypePattern (..))

augment :: Scheme Position Resolve scope -> Context scope -> Context (Local ':+ scope)
augment Scheme {implicit = False, parameters} = augmentWith parameters
augment Scheme {implicit = True} = augmentWith Strict.Vector.empty

augmentWith :: Strict.Vector (TypePattern position Resolve scope') -> Context scopes -> Context (Local ':+ scopes)
augmentWith parameters context = case shift context of
  context@Context {localTypes} -> context {localTypes = Map.fromList (zip (toList variables) indexes) <> localTypes}
  where
    variables = TypePattern.name <$> parameters
    indexes = Local.Local <$> [0 ..]

resolve :: Context scope -> Stage1.Scheme Position -> Scheme Position Resolve scope
resolve context (Stage1.Explicit {startPosition, parameters, constraints, result})
  | parameters <- TypePattern.resolve <$> parameters,
    context <- augmentWith parameters context =
      Scheme
        { startPosition,
          implicit,
          parameters,
          constraints = fmap (Constraint.resolve context) constraints,
          result = Type.resolve context result
        }
  where
    implicit = False
resolve context Stage1.Implicit {startPosition, constraints, result}
  | context <- augmentWith parameters context =
      Scheme
        { startPosition,
          implicit,
          parameters,
          constraints = fmap (Constraint.resolve context) constraints,
          result = Type.resolve context result
        }
  where
    implicit = True
    parameters = fabricate <$> names
    fabricate name =
      TypePattern
        { position = startPosition,
          name,
          typex = Inferred
        }
    names = Strict.Vector.fromList $ nubOrd $ filter (`Map.notMember` localTypes context) free
    free =
      map (\(_ :@ name) -> name) $
        mconcat
          [ foldMap Stage1.freeTypeVariables constraints,
            Stage1.freeTypeVariables result
          ]
