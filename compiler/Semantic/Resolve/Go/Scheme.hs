{-# LANGUAGE_HAZY UnorderedRecords #-}

module Semantic.Resolve.Go.Scheme where

import Data.Containers.ListUtils (nubOrd)
import Data.Foldable (toList)
import qualified Data.Map as Map
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import qualified Semantic.Index.Local as Local
import Semantic.Resolve.Context (Context (..))
import qualified Semantic.Resolve.Go.Constraints as Constraints
import qualified Semantic.Resolve.Go.Type as Type
import qualified Semantic.Resolve.Go.TypePattern as TypePattern (resolve)
import Semantic.Scope (Environment ((:+)), Local)
import Semantic.Shift (shift)
import Semantic.Stage (Resolve)
import Semantic.Tree.Combinators.Inferred (Inferred (..))
import Semantic.Tree.Scheme (Scheme (..))
import Semantic.Tree.TypePattern (TypePattern (TypePattern))
import qualified Semantic.Tree.TypePattern as TypePattern (TypePattern (..))
import qualified Syntax.FreeVariables as Syntax (FreeTypeVariables (..))
import Syntax.Position (Position)
import Syntax.Tree.Marked (Marked (..))
import qualified Syntax.Tree.Scheme as Syntax (Scheme (..))

augment :: Scheme Position Resolve scope -> Context scope -> Context (Local ':+ scope)
augment Scheme {implicit = False, parameters} = augmentWith parameters
augment Scheme {implicit = True} = augmentWith Strict.Vector.empty

augmentWith :: Strict.Vector (TypePattern position Resolve scope') -> Context scopes -> Context (Local ':+ scopes)
augmentWith parameters context = case shift context of
  context@Context {localTypes} -> context {localTypes = Map.fromList (zip (toList variables) indexes) <> localTypes}
  where
    variables = TypePattern.name <$> parameters
    indexes = Local.Local <$> [0 ..]

resolve :: Context scope -> Syntax.Scheme Position -> Scheme Position Resolve scope
resolve context (Syntax.Explicit {startPosition, parameters, constraints, result})
  | parameters <- TypePattern.resolve <$> parameters,
    context <- augmentWith parameters context =
      Scheme
        { startPosition,
          implicit,
          parameters,
          constraints = Constraints.resolve context constraints,
          result = Type.resolve context result
        }
  where
    implicit = False
resolve context Syntax.Implicit {startPosition, constraints, result}
  | context <- augmentWith parameters context =
      Scheme
        { startPosition,
          implicit,
          parameters,
          constraints = Constraints.resolve context constraints,
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
          [ Syntax.freeTypeVariables constraints,
            Syntax.freeTypeVariables result
          ]
