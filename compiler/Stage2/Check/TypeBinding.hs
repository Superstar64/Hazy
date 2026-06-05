module Stage2.Check.TypeBinding where

import Control.Monad.ST (ST)
import qualified Data.Kind
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Strict.Maybe as Strict (Maybe (..))
import qualified Data.Vector.Strict as Strict (Vector)
import Error (improperBindingGroup)
import Stage1.Position (Position)
import qualified Stage2.Connect as Connect
import qualified Stage2.Index.Link.Type as Type
import qualified Stage2.Index.Type0 as Type0
import qualified Stage2.Index.Type2 as Type2
import qualified Stage2.Label.Binding.Type as Label
import Stage2.Layout (Group)
import Stage2.Scope (Environment (..), GroupType, Local)
import Stage2.Shift (Category (Shift), Shift (..), shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Stage (Check)
import Stage2.Tree.TypeDeclaration (ungroupM)
import {-# SOURCE #-} Stage2.Tree.TypeDeclarationExtra (TypeDeclarationExtra)
import qualified Stage2.Tree.TypeDefinition2 as TypeDefinition2
import qualified Stage2.Check.Functor.Annotated as Functor (Annotated (..), NoLabel)
import {-# SOURCE #-} Stage2.Check.InstanceAnnotation (InstanceAnnotation)
import {-# SOURCE #-} qualified Stage2.Check.InstanceAnnotation as InstanceAnnotation
import {-# SOURCE #-} Stage2.Check.KindAnnotation (KindAnnotation)
import {-# SOURCE #-} qualified Stage2.Check.KindAnnotation as KindAnnotation
import {-# SOURCE #-} qualified Stage2.Check.Temporary.TypeDeclarationExtra as Temporary
import {-# SOURCE #-} Stage2.Check.Go.TypeDeclaration (TypeDeclaration)
import {-# SOURCE #-} qualified Stage2.Check.Go.TypeDeclaration as TypeDeclaration
import {-# SOURCE #-} qualified Stage2.Unify as Unify
import Stage4.Tree.Constraint (Constraint)
import qualified Stage4.Tree.Type as Simple (Type)
import qualified Stage4.Tree.Type as Simple.Type
import {-# SOURCE #-} qualified Stage4.Tree.TypeDeclaration as Simple (TypeDeclaration, simplify)
import {-# SOURCE #-} qualified Stage4.Tree.TypeDeclarationExtra as Simple (TypeDeclarationExtra)
import {-# SOURCE #-} qualified Stage4.Tree.TypeDeclarationExtra as SimpleExtra (simplify)

data Kind s scope
  = Wobbly !(Unify.Type s scope)
  | Rigid !(Simple.Type scope)

instance Shift (Kind s) where
  shift = \case
    Wobbly typex -> Wobbly (shift typex)
    Rigid typex -> Rigid (shift typex)

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

instance Shift (TypeBinding s) where
  shift TypeBinding {label, kind, synonym, extra, content, dataInstances, classInstances} =
    TypeBinding
      { label,
        kind = fmap shift kind,
        synonym = fmap (fmap (Shift.map (Shift.Over category))) synonym,
        content = fmap (Shift.map category) content,
        extra = fmap (Shift.map category) <$> extra,
        dataInstances = Map.map (fmap $ Shift.map category) $ Shift.mapInstances category dataInstances,
        classInstances = Map.map (fmap $ Shift.map category) $ Shift.mapInstances category classInstances
      }
    where
      category = Shift

rigid ::
  (Type.Link locality -> Type0.Index scope) ->
  (Type.Link locality -> ST s (TypeDefinition2.Set locality Check scope)) ->
  Functor.Annotated
    Label.TypeBinding
    (ST s (KindAnnotation scope))
    (ST s (TypeDeclaration locality Group Check scope)) ->
  ST s (TypeDeclarationExtra Group Check scope) ->
  Map (Type2.Index scope) (Functor.Annotated Functor.NoLabel (ST s (InstanceAnnotation scope)) b) ->
  Map (Type2.Index scope) (Functor.Annotated Functor.NoLabel (ST s (InstanceAnnotation scope)) d) ->
  TypeBinding s scope
rigid = bindingImpl (pure . SimpleExtra.simplify . Connect.seperate)

wobbly ::
  (Type.Link locality -> Type0.Index scope) ->
  (Type.Link locality -> ST s (TypeDefinition2.Set locality Check scope)) ->
  Functor.Annotated
    Label.TypeBinding
    (ST s (KindAnnotation scope))
    (ST s (TypeDeclaration locality Group Check scope)) ->
  ST s (Temporary.TypeDeclarationExtra s scope) ->
  Map (Type2.Index scope) (Functor.Annotated Functor.NoLabel (ST s (InstanceAnnotation scope)) b) ->
  Map (Type2.Index scope) (Functor.Annotated Functor.NoLabel (ST s (InstanceAnnotation scope)) d) ->
  TypeBinding s scope
wobbly = bindingImpl go
  where
    go extra = do
      extra <- Temporary.solve extra
      pure $ SimpleExtra.simplify $ Connect.seperate extra

group :: Position -> Label.TypeBinding scope -> Unify.Type s scopes -> TypeBinding s (GroupType ':+ scopes)
group position Label.TypeBinding {name, constructorNames} binding =
  TypeBinding
    { label = Label.TypeBinding {name, constructorNames},
      kind = pure $ Wobbly $ shift binding,
      content = abort,
      extra = abort,
      synonym = pure Strict.Nothing,
      dataInstances = abort,
      classInstances = abort
    }
  where
    abort :: a
    abort = improperBindingGroup position

bindingImpl ::
  (a -> Unify.Solve s (Simple.TypeDeclarationExtra scope)) ->
  (Type.Link locality -> Type0.Index scope) ->
  (Type.Link locality -> ST s (TypeDefinition2.Set locality Check scope)) ->
  Functor.Annotated
    Label.TypeBinding
    (ST s (KindAnnotation scope))
    (ST s (TypeDeclaration locality Group Check scope)) ->
  ST s a ->
  Map (Type2.Index scope) (Functor.Annotated label1 (ST s (InstanceAnnotation scope)) b1) ->
  Map (Type2.Index scope) (Functor.Annotated label2 (ST s (InstanceAnnotation scope)) b2) ->
  TypeBinding s scope
bindingImpl
  simpleExtra
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
        extra = simpleExtra <$> extra,
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

newtype Instance scope = Instance (Strict.Vector (Constraint scope))

instance Shift Instance where
  shift = shiftDefault

instance Shift.Functor Instance where
  map category (Instance constraints) = Instance (fmap (Shift.map category) constraints)
