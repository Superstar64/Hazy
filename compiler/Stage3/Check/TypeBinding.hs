module Stage3.Check.TypeBinding where

import Control.Monad.ST (ST)
import qualified Data.Kind
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Strict.Maybe as Strict (Maybe (..))
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Stage2.Index.Type2 as Type2
import qualified Stage2.Label.Binding.Type as Label
import Stage2.Scope (Environment (..), Local)
import Stage2.Shift (Shift (..), shiftDefault)
import qualified Stage2.Shift as Shift
import {-# SOURCE #-} Stage3.Check.InstanceAnnotation (InstanceAnnotation)
import {-# SOURCE #-} qualified Stage3.Check.InstanceAnnotation as InstanceAnnotation
import {-# SOURCE #-} Stage3.Check.KindAnnotation (KindAnnotation)
import {-# SOURCE #-} qualified Stage3.Check.KindAnnotation as KindAnnotation
import qualified Stage3.Functor.Annotated as Functor (Annotated (..), NoLabel)
import Stage3.Simple.Constraint (Constraint)
import {-# SOURCE #-} qualified Stage3.Simple.Type as Simple (Type)
import qualified Stage3.Simple.TypeDeclaration as Simple (TypeDeclaration, simplify)
import qualified Stage3.Simple.TypeDeclarationExtra as Simple (TypeDeclarationExtra)
import qualified Stage3.Simple.TypeDeclarationExtra as SimpleExtra (simplify)
import {-# SOURCE #-} Stage3.Tree.TypeDeclaration (TypeDeclaration)
import {-# SOURCE #-} qualified Stage3.Tree.TypeDeclaration as TypeDeclaration
import {-# SOURCE #-} Stage3.Tree.TypeDeclarationExtra (TypeDeclarationExtra)

type TypeBinding :: Data.Kind.Type -> Environment -> Data.Kind.Type
data TypeBinding s scope = TypeBinding
  { label :: !(forall scope. Label.TypeBinding scope),
    kind :: ST s (Simple.Type scope),
    content :: ST s (Simple.TypeDeclaration scope),
    extra :: ST s (Simple.TypeDeclarationExtra scope),
    synonym :: ST s (Strict.Maybe (Simple.Type (Local ':+ scope))),
    dataInstances :: Map (Type2.Index scope) (ST s (Instance scope)),
    classInstances :: Map (Type2.Index scope) (ST s (Instance scope))
  }

synonym_ :: TypeBinding s scope -> ST s (Strict.Maybe (Simple.Type (Local ':+ scope)))
synonym_ = synonym

instance Shift (TypeBinding s) where
  shift = shiftDefault

instance Shift.Functor (TypeBinding s) where
  map category TypeBinding {label, kind, synonym, extra, content, dataInstances, classInstances} =
    TypeBinding
      { label,
        kind = fmap (Shift.map category) kind,
        synonym = fmap (fmap (Shift.map (Shift.Over category))) synonym,
        content = fmap (Shift.map category) content,
        extra = fmap (Shift.map category) extra,
        dataInstances = Map.map (fmap $ Shift.map category) $ Shift.mapmap category dataInstances,
        classInstances = Map.map (fmap $ Shift.map category) $ Shift.mapmap category classInstances
      }

rigid ::
  Functor.Annotated
    Label.TypeBinding
    (ST s (KindAnnotation scope))
    (ST s (TypeDeclaration scope)) ->
  ST s (TypeDeclarationExtra scope) ->
  Map (Type2.Index scope) (Functor.Annotated Functor.NoLabel (ST s (InstanceAnnotation scope)) (ST s b)) ->
  Map (Type2.Index scope) (Functor.Annotated Functor.NoLabel (ST s (InstanceAnnotation scope)) (ST s d)) ->
  TypeBinding s scope
rigid
  Functor.Annotated
    { label,
      meta,
      content
    }
  extra
  dataInstances
  classInstances =
    TypeBinding
      { label,
        kind,
        synonym,
        content = Simple.simplify <$> content,
        extra = SimpleExtra.simplify <$> extra,
        dataInstances = Map.map (fmap (Instance . InstanceAnnotation.prerequisites'_) . Functor.meta) dataInstances,
        classInstances = Map.map (fmap (Instance . InstanceAnnotation.prerequisites'_) . Functor.meta) classInstances
      }
    where
      kind = do
        annotation <- meta
        case annotation of
          KindAnnotation.Annotation {kind'} -> pure kind'
          KindAnnotation.Inferred -> TypeDeclaration.kind'_ <$> content
          KindAnnotation.Synonym {kind'} -> pure kind'
      synonym = do
        annotation <- meta
        case annotation of
          KindAnnotation.Synonym {definition'} -> pure $ Strict.Just definition'
          _ -> pure Strict.Nothing

newtype Instance scope = Instance (Strict.Vector (Constraint scope))

instance Shift Instance where
  shift = shiftDefault

instance Shift.Functor Instance where
  map category (Instance constraints) = Instance (fmap (Shift.map category) constraints)
