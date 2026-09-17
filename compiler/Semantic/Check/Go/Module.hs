module Semantic.Check.Go.Module (Module (..), check) where

import Control.Monad.ST (ST)
import Data.Functor.Identity (Identity)
import qualified Data.Map as Map
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Void (Void)
import Graph.Topological (Loeb (..), loeb)
import qualified Graph.Topological as Topological
import Semantic.Check.Context (Context, globalBindings)
import qualified Semantic.Check.Go.Declarations as Declarations
import Semantic.Check.Go.Definition4 (now)
import Semantic.Functor2 (Proper (..))
import qualified Semantic.Index.Link.Term as Term
import qualified Semantic.Index.Link.Type as Link.Type
import qualified Semantic.Index.Type2 as Type2
import Semantic.Layout (Group)
import qualified Semantic.Locality as Locality
import Semantic.Scope (Global)
import Semantic.Stage (Check, Resolve)
import Semantic.Tree.Declaration (Declaration)
import Semantic.Tree.Declarations (Declarations (..))
import Semantic.Tree.Instance (Instance)
import Semantic.Tree.Module (Module (..), ModuleSet (..))
import Semantic.Tree.TypeDeclaration (TypeDeclaration)
import Prelude hiding (Functor)

lookupTerm ::
  Proper ModuleSet Group Check Global (ST s) ->
  Term.Link Locality.Global ->
  Declaration Identity Void Locality.Global (ST s) Group Check Global
lookupTerm (Proper (ModuleSet modules)) (Term.Global global local) =
  case modules Vector.! global of
    Module {declarations = Declarations {terms}} -> terms Vector.! local

lookupType ::
  Proper ModuleSet Group Check Global (ST s) ->
  Link.Type.Link Locality.Global ->
  TypeDeclaration Locality.Global (ST s) Group Check Global
lookupType (Proper (ModuleSet modules)) (Link.Type.Global global local) =
  case modules Vector.! global of
    Module {declarations = Declarations {types}} -> types Vector.! local

lookupDataInstance ::
  Int ->
  Proper ModuleSet Group Check Global (ST s) ->
  Int ->
  Type2.Index Global ->
  Instance Identity (ST s) Group Check Global
lookupDataInstance global (Proper (ModuleSet modules)) local index =
  case modules Vector.! global of
    Module {declarations = Declarations {dataInstances}} -> dataInstances Vector.! local Map.! index

lookupClassInstance ::
  Int ->
  Proper ModuleSet Group Check Global (ST s) ->
  Int ->
  Type2.Index Global ->
  Instance Identity (ST s) Group Check Global
lookupClassInstance global (Proper (ModuleSet modules)) local index =
  case modules Vector.! global of
    Module {declarations = Declarations {classInstances}} -> classInstances Vector.! local Map.! index

globals :: Proper ModuleSet Group Check Global (ST s) -> Context s Global
globals (Proper (ModuleSet modules)) = globalBindings modules

checkModule ::
  Int ->
  Module Identity Group Resolve Global ->
  Module (Topological.Formula (Proper ModuleSet Group Check Global) s) Group Check Global
checkModule global Module {name, declarations} =
  Module
    { name,
      declarations =
        Declarations.checkImpl
          Link.Type.unglobal
          (Term.Global global)
          (Link.Type.Global global)
          now
          lookupTerm
          lookupType
          (lookupDataInstance global)
          (lookupClassInstance global)
          globals
          declarations
    }

check :: Vector (Module Identity Group Resolve Global) -> Vector (Module Identity Group Check Global)
check modules = runModuleSet $ runProper $ loeb $ Loeb $ Proper $ ModuleSet $ Vector.imap checkModule modules
