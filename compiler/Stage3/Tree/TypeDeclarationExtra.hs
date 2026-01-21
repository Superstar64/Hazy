module Stage3.Tree.TypeDeclarationExtra where

import Control.Monad.ST (ST)
import qualified Data.Strict.Maybe as Strict (Maybe (..))
import Data.Traversable (for)
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import Stage2.Scope (Environment (..), Local)
import Stage2.Shift (shift)
import qualified Stage2.Tree.Method as Stage2 (Method (..))
import qualified Stage2.Tree.TypeDeclaration as Stage2 (TypeDeclaration (..))
import Stage3.Check.Context (Context)
import qualified Stage3.Simple.Constraint as Simple.Constraint
import qualified Stage3.Simple.Scheme as Simple (Scheme (..))
import qualified Stage3.Simple.SchemeOver as Simple (SchemeOver (..), augment, augment')
import qualified Stage3.Simple.Type as Simple.Type
import {-# SOURCE #-} qualified Stage3.Temporary.Definition as Definition
import Stage3.Tree.Definition (Definition)
import Stage3.Tree.Method (Method (..))
import Stage3.Tree.TypeDeclaration (TypeDeclaration)
import qualified Stage3.Tree.TypeDeclaration as TypeDeclaration

data TypeDeclarationExtra scope
  = ADT
  | Class
      { defaults :: !(Strict.Vector (Strict.Maybe (Definition (Local ':+ Local ':+ scope))))
      }
  | Synonym
  | GADT
  deriving (Show)

check ::
  Context s scope ->
  TypeDeclaration scope ->
  Stage2.TypeDeclaration scope ->
  ST s (TypeDeclarationExtra scope)
check context proper = \case
  Stage2.ADT {} -> pure ADT
  Stage2.Synonym {} -> pure Synonym
  Stage2.GADT {} -> pure GADT
  Stage2.Class {position, methods} -> case proper of
    TypeDeclaration.Class {parameter, constraints, methods = base} -> do
      context <-
        Simple.augment
          position
          (Strict.Vector.singleton parameter)
          (Simple.Constraint.simplify <$> constraints)
          context
      let go
            Method {annotation' = Simple.Scheme scheme@Simple.SchemeOver {result}}
            Stage2.Method {position, definition} = do
              for definition $ \definition -> do
                context <- Simple.augment' position scheme context
                definition <- Definition.check context (Simple.Type.lift result) (shift definition)
                Definition.solve definition
      defaults <- sequence $ Strict.Vector.zipWith go base methods
      pure Class {defaults}
    _ -> error "bad proper"
