module Semantic.Check.Context where

import Control.Monad.ST (ST)
import Core.Tree.Type (Type)
import qualified Data.Kind
import qualified Data.Strict.Maybe as Strict
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Semantic.Check.Functor.Annotated as Functor (Annotated (..))
import qualified Semantic.Check.Functor.Declarations as Functor (Declarations (..))
import qualified Semantic.Check.Functor.Module as Functor (Module (..))
import qualified Semantic.Check.Functor.ModuleSet as Functor (ModuleSet (..))
import {-# SOURCE #-} Semantic.Check.Go.TypeDeclaration (TypeDeclaration)
import {-# SOURCE #-} Semantic.Check.InstanceAnnotation (InstanceAnnotation)
import {-# SOURCE #-} Semantic.Check.KindAnnotation (KindAnnotation (..))
import Semantic.Check.LocalBinding (LocalBinding)
import qualified Semantic.Check.LocalBinding as LocalBinding
import {-# SOURCE #-} qualified Semantic.Check.Temporary.Declaration as Temporary (Declaration)
import {-# SOURCE #-} qualified Semantic.Check.Temporary.TypeDeclarationExtra as Temporary (TypeDeclarationExtra)
import Semantic.Check.TermBinding (TermBinding)
import qualified Semantic.Check.TermBinding as TermBinding
import {-# SOURCE #-} Semantic.Check.TypeAnnotation (TypeAnnotation)
import Semantic.Check.TypeBinding (TypeBinding (..))
import qualified Semantic.Check.TypeBinding as TypeBinding
import qualified Semantic.Index.Link.Type as Link.Type
import qualified Semantic.Index.Table.Local as Local
import qualified Semantic.Index.Table.Term as Term
import qualified Semantic.Index.Table.Type as Type
import qualified Semantic.Index.Type2 as Type2
import qualified Semantic.Label.Binding.Type as Label (TypeBinding)
import qualified Semantic.Label.Context as Label (Context (..))
import Semantic.Layout (Group)
import qualified Semantic.Locality as Locality
import Semantic.Scope (Environment (..), Global, Local)
import qualified Semantic.Scope as Scope (Declaration, GroupTerm, GroupType)
import qualified Semantic.Shift as Shift
import Semantic.Stage (Check)
import {-# SOURCE #-} Semantic.Tree.Declaration (Declaration)
import qualified Semantic.Tree.TypeDeclaration as Semantic (TypeDeclaration (..))
import {-# SOURCE #-} Semantic.Tree.TypeDeclarationExtra (TypeDeclarationExtra)
import qualified Semantic.Tree.TypeDefinition2 as Semantic (TypeDefinition2 (..))
import {-# SOURCE #-} qualified Semantic.Unify as Unify
import Syntax.Position (Position)

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
            Semantic.TypeDeclaration {definition} <- content
            case definition of
              _ Semantic.:::: set -> pure set
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
              Semantic.TypeDeclaration {definition} <- content
              case definition of
                _ Semantic.:::: set -> pure set
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
