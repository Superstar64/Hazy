module Stage3.Temporary.Declaration where

import Control.Monad.ST (ST)
import Stage1.Position (Position)
import Stage2.Layout (Normal)
import Stage2.Tree.Declaration (Key)
import qualified Stage2.Tree.Declaration as Stage2 (Declaration (..))
import qualified Stage2.Tree.Definition4 as Stage2 (Annotation (..), Definition4 (..))
import Stage3.Check.Context (Context (..))
import Stage3.Check.TypeAnnotation (Annotation (..), GlobalTypeAnnotation (..), LocalTypeAnnotation (..))
import qualified Stage3.Temporary.Definition3 as Definition3
import Stage3.Temporary.Definition4 (Definition4 (..))
import qualified Stage3.Temporary.Definition4 as Definition4
import qualified Stage3.Tree.Declaration as Solved (Declaration (..))
import Stage3.Tree.Definition4 (Annotation (..))
import qualified Stage3.Unify as Unify

data Declaration s scope
  = Declaration
  { position :: !Position,
    name :: !Key,
    definition :: !(Definition4 s scope)
  }

instance Unify.Zonk Declaration where
  zonk zonker = \case
    Declaration {position, name, definition} -> do
      definition <- Unify.zonk zonker definition
      pure
        Declaration
          { position,
            name,
            definition
          }

checkLocal ::
  Context s scope ->
  (Int -> ST s (Unify.Scheme s scope)) ->
  LocalTypeAnnotation s scope ->
  Stage2.Declaration locality Normal scope ->
  ST s (Declaration s scope)
checkLocal context shared annotation Stage2.Declaration {position, name, definition} = do
  definition <- definition `go` annotation
  pure
    Declaration
      { position,
        name,
        definition
      }
  where
    infix 0 `go`
    Stage2.Annotated {} Stage2.::: definition `go` LocalAnnotation Annotation {annotation} = do
      definition <- Definition3.check context shared (Definition3.Marked annotation) position definition
      pure $ Annotated annotation ::: definition
    Stage2.Inferred {} Stage2.::: definition `go` LocalInferred typex = do
      definition <- Definition3.check context shared (Definition3.Local typex) position definition
      pure $ Inferred ::: definition
    go _ _ = error "bad annotation"

checkGlobal ::
  Context s scope ->
  (Int -> ST s (Unify.Scheme s scope)) ->
  GlobalTypeAnnotation scope ->
  Stage2.Declaration locality Normal scope ->
  ST s (Declaration s scope)
checkGlobal context shared annotation Stage2.Declaration {position, name, definition} = do
  definition <- definition `go` annotation
  pure
    Declaration
      { position,
        name,
        definition
      }
  where
    infix 0 `go`
    Stage2.Annotated {} Stage2.::: definition `go` GlobalAnnotation Annotation {annotation} = do
      definition <- Definition3.check context shared (Definition3.Marked annotation) position definition
      pure $ Annotated annotation ::: definition
    Stage2.Inferred {} Stage2.::: definition `go` GlobalInferred = do
      definition <- Definition3.check context shared Definition3.Global position definition
      pure $ Inferred ::: definition
    go _ _ = error "bad annotation"

solve :: Declaration s scope -> ST s (Solved.Declaration scope)
solve Declaration {position, name, definition} = do
  definition <- Definition4.solve position definition
  pure Solved.Declaration {name, definition}
