module Stage3.Check.Context where

import Control.Monad.ST (ST)
import qualified Data.Kind
import qualified Data.Vector as Vector
import qualified Stage2.Index.Table.Local as Local
import qualified Stage2.Index.Table.Term as Term
import qualified Stage2.Index.Table.Type as Type
import qualified Stage2.Label.Context as Label
import Stage2.Scope (Declaration, Environment (..), Global)
import qualified Stage2.Shift as Shift
import {-# SOURCE #-} Stage3.Check.InstanceAnnotation (InstanceAnnotation)
import {-# SOURCE #-} Stage3.Check.KindAnnotation (KindAnnotation (..))
import Stage3.Check.LocalBinding (LocalBinding)
import qualified Stage3.Check.LocalBinding as LocalBinding
import Stage3.Check.TermBinding (TermBinding)
import qualified Stage3.Check.TermBinding as TermBinding
import {-# SOURCE #-} Stage3.Check.TypeAnnotation (TypeAnnotation)
import Stage3.Check.TypeBinding (TypeBinding)
import qualified Stage3.Check.TypeBinding as TypeBinding
import qualified Stage3.Functor.Declarations as Functor (Declarations (..))
import qualified Stage3.Functor.Module as Functor (Module (..))
import qualified Stage3.Functor.ModuleSet as Functor (ModuleSet (..))
import {-# SOURCE #-} Stage3.Tree.TermDeclaration (TermDeclaration)
import {-# SOURCE #-} Stage3.Tree.TypeDeclaration (TypeDeclaration)
import {-# SOURCE #-} qualified Stage3.Unify as Unify

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
    (ST s (TypeAnnotation () Global))
    (ST s (TermDeclaration Global))
    (ST s (KindAnnotation Global))
    (ST s (TypeDeclaration Global))
    (ST s x)
    (ST s (InstanceAnnotation Global))
    (ST s i) ->
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
    typeBindings Functor.Declarations {types, classInstances, dataInstances} =
      Vector.zipWith3 TypeBinding.rigid types dataInstances classInstances

localBindings ::
  Functor.Declarations
    (Declaration ':+ scope)
    (ST s (TypeAnnotation (Unify.Type s (Declaration ':+ scope)) (Declaration ':+ scope)))
    b
    (ST s (KindAnnotation (Declaration ':+ scope)))
    (ST s (TypeDeclaration (Declaration ':+ scope)))
    (ST s x)
    (ST s (InstanceAnnotation (Declaration ':+ scope)))
    (ST s i) ->
  Context s scope ->
  Context s (Declaration ':+ scope)
localBindings
  Functor.Declarations {terms, types, classInstances, dataInstances}
  Context {termEnvironment, localEnvironment, typeEnvironment} =
    Context
      { termEnvironment = Term.Declaration termBindings termEnvironment,
        localEnvironment = Local.Declaration localEnvironment,
        typeEnvironment = Type.Declaration typeBindings typeEnvironment
      }
    where
      termBindings = TermBinding.wobbly <$> terms
      typeBindings = Vector.zipWith3 TypeBinding.rigid types dataInstances classInstances

label :: Context s scope -> Label.Context scope
label Context {termEnvironment, localEnvironment, typeEnvironment} =
  Label.Context
    { terms = Term.map (error "term names are used in errors") termEnvironment,
      locals = Local.map (Local.Map LocalBinding.label) localEnvironment,
      types = Type.map (Type.Map TypeBinding.label) typeEnvironment
    }
