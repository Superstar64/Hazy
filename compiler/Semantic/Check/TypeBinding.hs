module Semantic.Check.TypeBinding where

import Control.Monad.ST (ST)
import Core.Tree.Constraints (Constraints)
import qualified Core.Tree.Type as Simple (Type)
import qualified Core.Tree.Type as Simple.Type
import {-# SOURCE #-} qualified Core.Tree.TypeDeclaration as Simple (TypeDeclaration, simplify)
import {-# SOURCE #-} qualified Core.Tree.TypeDeclarationExtra as Simple (TypeDeclarationExtra)
import {-# SOURCE #-} qualified Core.Tree.TypeDeclarationExtra as SimpleExtra (simplify)
import Core.Type.Functor (shiftLogical)
import qualified Core.Type.Functor as Core
import Data.Functor.Identity (Identity (..))
import qualified Data.Kind
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Strict.Maybe as Strict (Maybe (..))
import Error (improperBindingGroup)
import qualified Semantic.Check.Functor.Annotated as Functor (Annotated (..), NoLabel)
import {-# SOURCE #-} Semantic.Check.Go.TypeDeclaration (TypeDeclaration)
import {-# SOURCE #-} qualified Semantic.Check.Go.TypeDeclaration as TypeDeclaration
import {-# SOURCE #-} Semantic.Check.InstanceAnnotation (InstanceAnnotation)
import {-# SOURCE #-} qualified Semantic.Check.InstanceAnnotation as InstanceAnnotation
import {-# SOURCE #-} Semantic.Check.KindAnnotation (KindAnnotation)
import {-# SOURCE #-} qualified Semantic.Check.KindAnnotation as KindAnnotation
import qualified Semantic.Connect as Connect
import qualified Semantic.Index.Link.Type as Type
import qualified Semantic.Index.Type0 as Type0
import qualified Semantic.Index.Type2 as Type2
import qualified Semantic.Label.Binding.Type as Label
import Semantic.Layout (Group)
import Semantic.Scope (Environment (..), GroupType, Local)
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Semantic.Stage (Check)
import Semantic.Tree.TypeDeclaration (ungroupM)
import {-# SOURCE #-} Semantic.Tree.TypeDeclarationExtra (TypeDeclarationExtra)
import qualified Semantic.Tree.TypeGroup as TypeGroup
import Semantic.Unify (Solve)
import qualified Semantic.Unify as Unify
import Syntax.Position (Position)

data Kind s scope
  = Wobbly !(Unify.Type s scope)
  | Rigid !(Simple.Type scope)

instance Shift0.Functor (Kind s) where
  map category = \case
    Wobbly typex -> Wobbly (Core.mapLogical category typex)
    Rigid typex -> Rigid (Shift0.map category typex)

type TypeBinding :: Data.Kind.Type -> Environment -> Data.Kind.Type
data TypeBinding s scope = TypeBinding
  { label :: !(forall scope. Label.TypeBinding scope),
    kind :: ST s (Kind s scope),
    content :: ST s (Simple.TypeDeclaration scope),
    extra :: ST s (Unify.Solve s (Simple.TypeDeclarationExtra scope)),
    synonym :: ST s (Strict.Maybe (Simple.Type (Local ':+ scope))),
    dataInstances :: Map (Type2.Index scope) (ST s (Instance scope)),
    classInstances :: Map (Type2.Index scope) (ST s (Instance scope))
  }

synonym_ :: TypeBinding s scope -> ST s (Strict.Maybe (Simple.Type (Local ':+ scope)))
synonym_ = synonym

instance Shift0.Functor (TypeBinding s) where
  map category0 TypeBinding {label, kind, synonym, extra, content, dataInstances, classInstances} =
    TypeBinding
      { label,
        kind = fmap (Shift0.map category0) kind,
        synonym = fmap (fmap (Shift.map (Shift.Over category))) synonym,
        content = fmap (Shift.map category) content,
        extra = fmap (Shift.map category) <$> extra,
        dataInstances = Map.map (fmap $ Shift.map category) $ Shift.mapInstances category dataInstances,
        classInstances = Map.map (fmap $ Shift.map category) $ Shift.mapInstances category classInstances
      }
    where
      category = Shift.lift category0

binding ::
  (Type.Link locality -> Type0.Index scope) ->
  (Type.Link locality -> ST s (TypeGroup.Set locality Check scope)) ->
  Functor.Annotated
    Label.TypeBinding
    (ST s (KindAnnotation scope))
    (ST s (TypeDeclaration locality Identity Group Check scope)) ->
  ST s (Identity (Solve s (TypeDeclarationExtra Group Check scope))) ->
  Map (Type2.Index scope) (Functor.Annotated Functor.NoLabel (ST s (InstanceAnnotation scope)) b) ->
  Map (Type2.Index scope) (Functor.Annotated Functor.NoLabel (ST s (InstanceAnnotation scope)) d) ->
  TypeBinding s scope
binding
  index
  lookup
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
        content = do
          definition <- content
          definition <- ungroupM index lookup definition
          pure $ Simple.simplify definition,
        extra = fmap (SimpleExtra.simplify . Connect.seperate) <$> fmap runIdentity extra,
        dataInstances = Map.map (fmap (Instance . InstanceAnnotation.prerequisites'_) . Functor.meta) dataInstances,
        classInstances = Map.map (fmap (Instance . InstanceAnnotation.prerequisites'_) . Functor.meta) classInstances
      }
    where
      kind = do
        annotation <- meta
        Rigid <$> case annotation of
          KindAnnotation.Annotation {kind} -> pure kind
          KindAnnotation.Inferred -> TypeDeclaration.kind' <$> content
          KindAnnotation.Synonym {kind} -> pure kind
      synonym = do
        annotation <- meta
        case annotation of
          KindAnnotation.Synonym {synonym} -> pure $ Strict.Just (Simple.Type.simplify synonym)
          _ -> pure Strict.Nothing

group :: Position -> Label.TypeBinding scope -> Unify.Type s scopes -> TypeBinding s (GroupType ':+ scopes)
group position Label.TypeBinding {name, constructorNames} binding =
  TypeBinding
    { label = Label.TypeBinding {name, constructorNames},
      kind = pure $ Wobbly $ shiftLogical binding,
      content = abort,
      extra = abort,
      synonym = pure Strict.Nothing,
      dataInstances = abort,
      classInstances = abort
    }
  where
    abort :: a
    abort = improperBindingGroup position

newtype Instance scope = Instance (Constraints scope)

instance Shift0.Functor Instance where
  map = Shift.mapDefault

instance Shift.Functor Instance where
  map category (Instance constraints) = Instance (Shift.map category constraints)
