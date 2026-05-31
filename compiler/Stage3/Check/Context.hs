module Stage3.Check.Context where

import Control.Monad.ST (ST)
import qualified Data.Kind
import qualified Data.Strict.Maybe as Strict
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Stage1.Position (Position)
import qualified Stage2.Index.Link.Type as Link.Type
import qualified Stage2.Index.Table.Local as Local
import qualified Stage2.Index.Table.Term as Term
import qualified Stage2.Index.Table.Type as Type
import qualified Stage2.Index.Type2 as Type2
import qualified Stage2.Label.Binding.Type as Label (TypeBinding)
import qualified Stage2.Label.Context as Label (Context (..))
import Stage2.Layout (Group)
import qualified Stage2.Locality as Locality
import Stage2.Scope (Environment (..), Global, Local)
import qualified Stage2.Scope as Scope (Declaration, GroupTerm, GroupType)
import qualified Stage2.Shift as Shift
import Stage2.Stage (Check)
import {-# SOURCE #-} Stage2.Tree.Declaration (Declaration)
import qualified Stage2.Tree.TypeDeclaration as Stage2 (TypeDeclaration (..))
import {-# SOURCE #-} Stage2.Tree.TypeDeclarationExtra (TypeDeclarationExtra)
import qualified Stage2.Tree.TypeDefinition2 as Stage2 (TypeDefinition2 (..))
import {-# SOURCE #-} Stage3.Check.InstanceAnnotation (InstanceAnnotation)
import {-# SOURCE #-} Stage3.Check.KindAnnotation (KindAnnotation (..))
import Stage3.Check.LocalBinding (LocalBinding)
import qualified Stage3.Check.LocalBinding as LocalBinding
import Stage3.Check.TermBinding (TermBinding)
import qualified Stage3.Check.TermBinding as TermBinding
import {-# SOURCE #-} Stage3.Check.TypeAnnotation (TypeAnnotation)
import Stage3.Check.TypeBinding (TypeBinding (..))
import qualified Stage3.Check.TypeBinding as TypeBinding
import qualified Stage3.Functor.Annotated as Functor (Annotated (..))
import qualified Stage3.Functor.Declarations as Functor (Declarations (..))
import qualified Stage3.Functor.Module as Functor (Module (..))
import qualified Stage3.Functor.ModuleSet as Functor (ModuleSet (..))
import {-# SOURCE #-} qualified Stage3.Temporary.Declaration as Temporary (Declaration)
import {-# SOURCE #-} qualified Stage3.Temporary.TypeDeclarationExtra as Temporary (TypeDeclarationExtra)
import {-# SOURCE #-} Stage3.Tree.TypeDeclaration (TypeDeclaration)
import {-# SOURCE #-} qualified Stage3.Unify as Unify
import Stage4.Tree.Type (Type)

type Context :: Data.Kind.Type -> Environment -> Data.Kind.Type
data Context s scope = Context
  { termEnvironment :: !(Term.Table (TermBinding s) scope),
    localEnvironment :: !(Local.Table (LocalBinding s) scope),
    typeEnvironment :: !(Type.Table (TypeBinding s) scope)
  }

typeEnvironment_ = typeEnvironment

instance Shift.Unshift (Context s) where
  unshift Context {termEnvironment, localEnvironment, typeEnvironment} =
    Context
      { termEnvironment = Shift.unshift termEnvironment,
        localEnvironment = Shift.unshift localEnvironment,
        typeEnvironment = Shift.unshift typeEnvironment
      }

globalBindings ::
  Functor.ModuleSet
    (ST s (TypeAnnotation Global))
    (ST s (Declaration Locality.Global Group Check Global))
    (ST s (KindAnnotation Global))
    (ST s (TypeDeclaration Locality.Global Group Check Global))
    (ST s (TypeDeclarationExtra Group Check Global))
    (ST s (InstanceAnnotation Global))
    x ->
  Context s Global
globalBindings (Functor.ModuleSet modules) =
  Context
    { termEnvironment = Term.Global $ types <$> modules,
      localEnvironment = Local.Global,
      typeEnvironment = Type.Global $ kinds <$> modules
    }
  where
    types Functor.Module {declarations} = termBindings declarations
    kinds Functor.Module {declarations} = typeBindings declarations
    termBindings Functor.Declarations {terms} =
      TermBinding.rigid <$> terms
    typeBindings Functor.Declarations {types, typeExtras, classInstances, dataInstances} =
      Vector.zipWith4 go types typeExtras dataInstances classInstances
    go = TypeBinding.rigid Link.Type.unglobal $ \case
      Link.Type.Global global local
        | Functor.Module {declarations} <- modules Vector.! global,
          Functor.Declarations {types} <- declarations,
          Functor.Annotated {content} <- types Vector.! local -> do
            Stage2.TypeDeclaration {definition} <- content
            case definition of
              Stage2.Group set -> pure set
              _ -> error "bad definition lookup"

localBindings ::
  Functor.Declarations
    (Scope.Declaration ':+ scope)
    (ST s (TypeAnnotation (Scope.Declaration ':+ scope)))
    (ST s (Temporary.Declaration Locality.Local s (Scope.Declaration ':+ scope)))
    (ST s (KindAnnotation (Scope.Declaration ':+ scope)))
    (ST s (TypeDeclaration Locality.Local Group Check (Scope.Declaration ':+ scope)))
    (ST s (Temporary.TypeDeclarationExtra s (Scope.Declaration ':+ scope)))
    (ST s (InstanceAnnotation (Scope.Declaration ':+ scope)))
    x' ->
  Context s scope ->
  Context s (Scope.Declaration ':+ scope)
localBindings
  Functor.Declarations {terms, types, typeExtras, classInstances, dataInstances}
  Context {termEnvironment, localEnvironment, typeEnvironment} =
    Context
      { termEnvironment = Term.Declaration termBindings termEnvironment,
        localEnvironment = Local.Declaration localEnvironment,
        typeEnvironment = Type.Declaration typeBindings typeEnvironment
      }
    where
      termBindings = TermBinding.wobbly <$> terms
      typeBindings = Vector.zipWith4 go types typeExtras dataInstances classInstances
      go = TypeBinding.wobbly Link.Type.unlocal $ \case
        Link.Type.Declaration local
          | Functor.Annotated {content} <- types Vector.! local -> do
              Stage2.TypeDeclaration {definition} <- content
              case definition of
                Stage2.Group set -> pure set
                _ -> error "bad definition lookup"

groupTermBindings ::
  Vector (Unify.Type s scope) ->
  Context s scope ->
  Context s (Scope.GroupTerm ':+ scope)
groupTermBindings declarations Context {termEnvironment, localEnvironment, typeEnvironment} =
  Context
    { termEnvironment = Term.GroupTerm termBindings termEnvironment,
      localEnvironment = Local.GroupTerm localEnvironment,
      typeEnvironment = Type.GroupTerm typeEnvironment
    }
  where
    termBindings = TermBinding.group <$> declarations

groupTypeBindings ::
  Vector Position ->
  Vector (Label.TypeBinding scope') ->
  Vector (Unify.Type s scope) ->
  Context s scope ->
  Context s (Scope.GroupType ':+ scope)
groupTypeBindings positions labels declarations Context {termEnvironment, localEnvironment, typeEnvironment} =
  Context
    { termEnvironment = Term.GroupType termEnvironment,
      localEnvironment = Local.GroupType localEnvironment,
      typeEnvironment = Type.GroupType typeBindings typeEnvironment
    }
  where
    typeBindings = Vector.zipWith3 TypeBinding.group positions labels declarations

label :: Context s scope -> Label.Context scope
label Context {termEnvironment, localEnvironment, typeEnvironment} =
  Label.Context
    { terms = Term.map (error "term names are used in errors") termEnvironment,
      locals = Local.map (Local.Map LocalBinding.label) localEnvironment,
      types = Type.map (Type.Map TypeBinding.label) typeEnvironment
    }

lookupSynonym :: Context s scope -> Type2.Index scope -> ST s (Strict.Maybe (Type (Local ':+ scope)))
lookupSynonym Context {typeEnvironment} (Type2.Index index) = do
  let TypeBinding {synonym} = typeEnvironment Type.! index
  synonym
lookupSynonym _ _ = pure Strict.Nothing
