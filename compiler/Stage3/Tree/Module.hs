module Stage3.Tree.Module (Module (..), check) where

import qualified Data.Map as Map
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Error (cyclicalTypeChecking)
import Graph.Topological (Formula7 (..), Loeb7 (..), loeb7)
import qualified Graph.Topological7
import qualified Stage2.Index.Type as Type
import Stage2.Layout (Normal)
import qualified Stage2.Locality as Locality
import Stage2.Scope (Global)
import Stage2.Stage (Check, Resolve)
import Stage2.Tree.Declaration (Declaration (..))
import qualified Stage2.Tree.Declaration as Declaration
import qualified Stage2.Tree.Declaration as Stage2 (Declaration)
import qualified Stage2.Tree.Declaration as Stage2.Declaration
import qualified Stage2.Tree.Declarations as Stage2.Declarations
import Stage2.Tree.Instance (Instance)
import qualified Stage2.Tree.Instance as Stage2.Instance
import Stage2.Tree.Module (Module (..))
import qualified Stage2.Tree.TypeDeclaration as Stage2.TypeDeclaration
import Stage2.Tree.TypeDeclarationExtra (TypeDeclarationExtra)
import qualified Stage2.Tree.TypeDeclarationExtra as Stage2.TypeDeclarationExtra
import Stage3.Check.Context (globalBindings)
import Stage3.Check.InstanceAnnotation (InstanceAnnotation)
import qualified Stage3.Check.InstanceAnnotation as InstanceAnnotation
import Stage3.Check.KindAnnotation (KindAnnotation)
import qualified Stage3.Check.KindAnnotation as KindAnnotation
import Stage3.Check.TypeAnnotation (GlobalTypeAnnotation)
import qualified Stage3.Check.TypeAnnotation as TypeAnnotation
import qualified Stage3.Functor.Annotated as Functor (Annotated (..))
import qualified Stage3.Functor.Declarations as Functor (Declarations (..))
import qualified Stage3.Functor.Instance.Key as Instance.Key
import Stage3.Functor.Module (fromStage2)
import qualified Stage3.Functor.Module as Functor (Module (..))
import Stage3.Functor.ModuleSet (mapWithKey)
import qualified Stage3.Functor.ModuleSet as Functor (ModuleSet (..))
import qualified Stage3.Temporary.Declaration as Declaration.Unsolved
import qualified Stage3.Temporary.Instance as Instance (Key (..), check, solve)
import qualified Stage3.Temporary.TypeDeclarationExtra as TypeDeclarationExtra
import qualified Stage3.Tree.Declarations as Declarations
import Stage3.Tree.TypeDeclaration (TypeDeclaration)
import qualified Stage3.Tree.TypeDeclaration as TypeDeclaration
import Prelude hiding (Functor)

type Formula s z =
  Formula7
    Functor.ModuleSet
    s
    (GlobalTypeAnnotation Global)
    (Declaration Locality.Global Normal Check Global)
    (KindAnnotation Global)
    (TypeDeclaration Locality.Global Normal Check Global)
    (TypeDeclarationExtra Normal Check Global)
    (InstanceAnnotation Global)
    (Instance Normal Check Global)
    z

fromFunctor ::
  Functor.Module
    a
    (Declaration Locality.Global Normal Check Global)
    b
    (TypeDeclaration Locality.Global Normal Check Global)
    (TypeDeclarationExtra Normal Check Global)
    c
    (Instance Normal Check Global) ->
  Module Normal Check
fromFunctor (Functor.Module {name, declarations}) =
  Module
    { name,
      declarations = Declarations.fromFunctor declarations
    }

fromFunctors ::
  Functor.ModuleSet
    a
    (Declaration Locality.Global Normal Check Global)
    b
    (TypeDeclaration Locality.Global Normal Check Global)
    (TypeDeclarationExtra Normal Check Global)
    c
    (Instance Normal Check Global) ->
  Vector (Module Normal Check)
fromFunctors (Functor.ModuleSet modules) = fmap fromFunctor modules

check :: Vector (Module Normal Resolve) -> Vector (Module Normal Check)
check modules =
  fromFunctors
    $ mapWithKey
      pass
      term
      pass
      typex
      pass
      pass
      pass
    $ loeb7
    $ Loeb7
    $ mapWithKey
      checkTermAnnotation
      checkTermDeclaration
      checkTypeAnnotation
      checkTypeDeclaration
      checkTypeDeclarationExtra
      checkInstanceAnnotation
      checkInstanceDeclaration
      (Functor.ModuleSet $ fmap fromStage2 modules)
  where
    pass :: a -> b -> c -> c
    pass = const $ const id

    term modulex term declaration
      | modulex <- modules Vector.! modulex,
        declarations <- declarations modulex,
        terms <- Stage2.Declarations.terms declarations,
        term <- terms Vector.! term =
          Declaration.lazy term declaration

    typex modulex typex declaration
      | modulex <- modules Vector.! modulex,
        declarations <- declarations modulex,
        types <- Stage2.Declarations.types declarations,
        typex <- types Vector.! typex =
          TypeDeclaration.lazy typex declaration

checkTermAnnotation ::
  p1 ->
  p2 ->
  Stage2.Declaration locality Normal Resolve Global ->
  Formula s (GlobalTypeAnnotation Global)
checkTermAnnotation _ _ declaration = Formula7 {cycle, run}
  where
    cycle :: a
    cycle = cyclicalTypeChecking $ Stage2.Declaration.position declaration
    run modules = TypeAnnotation.checkGlobal (globalBindings modules) declaration

checkTermDeclaration ::
  Int ->
  Int ->
  Stage2.Declaration locality Normal Resolve Global ->
  Formula s (Declaration locality Normal Check Global)
checkTermDeclaration global local declaration = Formula7 {cycle, run}
  where
    cycle :: a
    cycle = cyclicalTypeChecking $ Stage2.Declaration.position declaration
    run moduleSet@(Functor.ModuleSet modules) = do
      let Functor.Module {declarations} = modules Vector.! global
          Functor.Declarations {terms} = declarations
          Functor.Annotated {meta} = terms Vector.! local
      annotation <- meta
      let context = globalBindings moduleSet
      unsolved <- Declaration.Unsolved.checkGlobal context annotation declaration
      Declaration.Unsolved.solve unsolved

checkTypeAnnotation ::
  p1 ->
  p2 ->
  Stage2.TypeDeclaration.TypeDeclaration locality Normal Resolve Global ->
  Formula s (KindAnnotation Global)
checkTypeAnnotation _ _ declaration = Formula7 {cycle, run}
  where
    cycle :: a
    cycle = cyclicalTypeChecking $ Stage2.TypeDeclaration.position declaration
    run modules = KindAnnotation.check (globalBindings modules) declaration

checkTypeDeclaration ::
  Int ->
  Int ->
  Stage2.TypeDeclaration.TypeDeclaration locality Normal Resolve Global ->
  Formula s (TypeDeclaration locality Normal Check Global)
checkTypeDeclaration global local declaration = Formula7 {cycle, run}
  where
    cycle :: a
    cycle = cyclicalTypeChecking $ Stage2.TypeDeclaration.position declaration
    run moduleSet@(Functor.ModuleSet modules) = do
      let Functor.Module {declarations} = modules Vector.! global
          Functor.Declarations {types} = declarations
          Functor.Annotated {meta} = types Vector.! local
      annotation <- meta
      let context = globalBindings moduleSet
      -- todo, augment context with self to allow basic recursive inference
      TypeDeclaration.check context annotation declaration

checkTypeDeclarationExtra ::
  Int ->
  Int ->
  Stage2.TypeDeclarationExtra.TypeDeclarationExtra Normal Resolve Global ->
  Formula s (TypeDeclarationExtra Normal Check Global)
checkTypeDeclarationExtra global local declaration = Formula7 {cycle, run}
  where
    cycle :: a
    cycle = cyclicalTypeChecking $ Stage2.TypeDeclarationExtra.position declaration
    run moduleSet@(Functor.ModuleSet modules) = do
      let Functor.Module {declarations} = modules Vector.! global
          Functor.Declarations {types} = declarations
          Functor.Annotated {content} = types Vector.! local
      proper <- content
      let context = globalBindings moduleSet
      extra <- TypeDeclarationExtra.check context (Type.Global global local) proper declaration
      TypeDeclarationExtra.solve extra

checkInstanceAnnotation ::
  p1 ->
  p2 ->
  Stage2.Instance.Instance Normal Resolve Global ->
  Formula s (InstanceAnnotation Global)
checkInstanceAnnotation _ _ declaration = Formula7 {cycle, run}
  where
    cycle :: a
    cycle = cyclicalTypeChecking $ Stage2.Instance.startPosition declaration
    run modules = InstanceAnnotation.check (globalBindings modules) declaration

checkInstanceDeclaration ::
  Int ->
  Instance.Key.Key Global ->
  Stage2.Instance.Instance Normal Resolve Global ->
  Formula s (Instance Normal Check Global)
checkInstanceDeclaration global key declaration = Formula7 {cycle, run}
  where
    cycle :: a
    cycle = cyclicalTypeChecking $ Stage2.Instance.startPosition declaration
    run moduleSet@(Functor.ModuleSet modules) = do
      let Functor.Module {declarations} = modules Vector.! global
          Functor.Declarations {dataInstances, classInstances} = declarations
      case key of
        Instance.Key.Data {index, classKey} -> do
          let Functor.Annotated {meta} = dataInstances Vector.! index Map.! classKey
              key = Instance.Data {index1 = classKey, head1 = Type.Global global index}
          annotation <- meta
          instancex <- Instance.check (globalBindings moduleSet) key annotation declaration
          Instance.solve instancex
        Instance.Key.Class {index, dataKey} -> do
          let Functor.Annotated {meta} = classInstances Vector.! index Map.! dataKey
              key = Instance.Class {index2 = Type.Global global index, head2 = dataKey}
          annotation <- meta
          instancex <- Instance.check (globalBindings moduleSet) key annotation declaration
          Instance.solve instancex
