module Semantic.Check.Go.Module (Module (..), check) where

import Control.Monad.ST (ST)
import Core.Tree.Scheme (Scheme (..))
import qualified Core.Tree.SchemeOver as Scheme
import qualified Core.Tree.Type as Simple
import qualified Data.Map as Map
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Strict as Strict.Vector
import Error (cyclicalTypeChecking)
import Graph.Topological (Formula7 (..), Loeb7 (..), loeb7)
import qualified Graph.Topological7
import Semantic.Check.Context (globalBindings)
import qualified Semantic.Check.Functor.Annotated as Functor (Annotated (..))
import qualified Semantic.Check.Functor.Declarations as Functor (Declarations (..))
import qualified Semantic.Check.Functor.Instance.Key as Instance.Key
import Semantic.Check.Functor.Module (fromStage2)
import qualified Semantic.Check.Functor.Module as Functor (Module (..))
import Semantic.Check.Functor.ModuleSet (mapWithKey)
import qualified Semantic.Check.Functor.ModuleSet as Functor (ModuleSet (..))
import qualified Semantic.Check.Go.Declarations as Declarations
import Semantic.Check.Go.TypeDeclaration (TypeDeclaration (..))
import qualified Semantic.Check.Go.TypeDeclaration as TypeDeclaration
import Semantic.Check.InstanceAnnotation (InstanceAnnotation)
import qualified Semantic.Check.InstanceAnnotation as InstanceAnnotation
import Semantic.Check.KindAnnotation (KindAnnotation)
import qualified Semantic.Check.KindAnnotation as KindAnnotation
import qualified Semantic.Check.Simple.Scheme as Simple.Scheme
import qualified Semantic.Check.Temporary.Declaration as Declaration.Unsolved
import qualified Semantic.Check.Temporary.Instance as Instance (Key (..), check, solve)
import qualified Semantic.Check.Temporary.TypeDeclarationExtra as TypeDeclarationExtra
import Semantic.Check.TypeAnnotation (TypeAnnotation)
import qualified Semantic.Check.TypeAnnotation as TypeAnnotation
import qualified Semantic.Index.Link.Term as Link.Term
import qualified Semantic.Index.Link.Type as Link.Type
import qualified Semantic.Index.Type as Type
import Semantic.Layout (Group)
import qualified Semantic.Locality as Locality
import Semantic.Scope (Global)
import Semantic.Stage (Check, Resolve)
import Semantic.Tree.Combinators.Inferred (Inferred (..))
import Semantic.Tree.Declaration (Declaration (..))
import qualified Semantic.Tree.Declaration as Declaration
import qualified Semantic.Tree.Declaration as Semantic (Declaration)
import qualified Semantic.Tree.Declaration as Semantic.Declaration
import qualified Semantic.Tree.Declarations as Semantic.Declarations
import qualified Semantic.Tree.Definition4 as Definition4
import Semantic.Tree.Instance (Instance)
import qualified Semantic.Tree.Instance as Semantic.Instance
import Semantic.Tree.Module (Module (..))
import qualified Semantic.Tree.TypeDeclaration as Semantic.TypeDeclaration
import Semantic.Tree.TypeDeclarationExtra (TypeDeclarationExtra)
import qualified Semantic.Tree.TypeDeclarationExtra as Semantic.TypeDeclarationExtra
import qualified Semantic.Tree.TypeDefinition2 as TypeDefinition2
import qualified Semantic.Unify as Unify
import Prelude hiding (Functor)

type Formula s z =
  Formula7
    Functor.ModuleSet
    s
    (TypeAnnotation Global)
    (Declaration Locality.Global Group Check Global)
    (KindAnnotation Global)
    (TypeDeclaration Locality.Global Group Check Global)
    (TypeDeclarationExtra Group Check Global)
    (InstanceAnnotation Global)
    (Instance Group Check Global)
    z

fromFunctor ::
  Functor.Module
    a
    (Declaration Locality.Global Group Check Global)
    b
    (TypeDeclaration Locality.Global Group Check Global)
    (TypeDeclarationExtra Group Check Global)
    c
    (Instance Group Check Global) ->
  Module Group Check
fromFunctor (Functor.Module {name, declarations}) =
  Module
    { name,
      declarations = Declarations.fromFunctor declarations
    }

fromFunctors ::
  Functor.ModuleSet
    a
    (Declaration Locality.Global Group Check Global)
    b
    (TypeDeclaration Locality.Global Group Check Global)
    (TypeDeclarationExtra Group Check Global)
    c
    (Instance Group Check Global) ->
  Vector (Module Group Check)
fromFunctors (Functor.ModuleSet modules) = fmap fromFunctor modules

check :: Vector (Module Group Resolve) -> Vector (Module Group Check)
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
        terms <- Semantic.Declarations.terms declarations,
        term <- terms Vector.! term =
          Declaration.lazy term declaration

    typex modulex typex declaration
      | modulex <- modules Vector.! modulex,
        declarations <- declarations modulex,
        types <- Semantic.Declarations.types declarations,
        typex <- types Vector.! typex =
          TypeDeclaration.lazy typex declaration

checkTermAnnotation ::
  p1 ->
  p2 ->
  Semantic.Declaration locality Group Resolve Global ->
  Formula s (TypeAnnotation Global)
checkTermAnnotation _ _ declaration = Formula7 {cycle, run}
  where
    cycle :: a
    cycle = cyclicalTypeChecking $ Semantic.Declaration.position declaration
    run modules = TypeAnnotation.check (globalBindings modules) declaration

checkTermDeclaration ::
  forall s.
  Int ->
  Int ->
  Semantic.Declaration Locality.Global Group Resolve Global ->
  Formula s (Declaration Locality.Global Group Check Global)
checkTermDeclaration global local declaration = Formula7 {cycle, run}
  where
    cycle :: a
    cycle = cyclicalTypeChecking $ Semantic.Declaration.position declaration
    run moduleSet@(Functor.ModuleSet modules) = do
      let Functor.Module {declarations} = modules Vector.! global
          Functor.Declarations {terms} = declarations
          Functor.Annotated {meta} = terms Vector.! local
          link :: Link.Term.Link Locality.Global -> Int -> ST s (Unify.Scheme s Global)
          link (Link.Term.Global global local) id = do
            let Functor.Module {declarations} = modules Vector.! global
                Functor.Declarations {terms} = declarations
                Functor.Annotated {content} = terms Vector.! local
            Declaration {definition} <- content
            pure $ case definition of
              Solved types Definition4.:::: _ -> Simple.Scheme.lift $ Scheme $ Scheme.map go types
                where
                  go = Scheme.Map $ \case
                    Definition4.Types types -> types Strict.Vector.! id
              _ -> error "bad link lookup"
      annotation <- meta
      let context = globalBindings moduleSet
      unsolved <- Declaration.Unsolved.check context link annotation declaration
      Unify.runSolve $ Declaration.Unsolved.solve unsolved

checkTypeAnnotation ::
  p1 ->
  p2 ->
  Semantic.TypeDeclaration.TypeDeclaration locality Group Resolve Global ->
  Formula s (KindAnnotation Global)
checkTypeAnnotation _ _ declaration = Formula7 {cycle, run}
  where
    cycle :: a
    cycle = cyclicalTypeChecking $ Semantic.TypeDeclaration.position declaration
    run modules = KindAnnotation.check (globalBindings modules) declaration

checkTypeDeclaration ::
  forall s.
  Int ->
  Int ->
  Semantic.TypeDeclaration.TypeDeclaration Locality.Global Group Resolve Global ->
  Formula s (TypeDeclaration Locality.Global Group Check Global)
checkTypeDeclaration global local declaration = Formula7 {cycle, run}
  where
    cycle :: a
    cycle = cyclicalTypeChecking $ Semantic.TypeDeclaration.position declaration
    run moduleSet@(Functor.ModuleSet modules) = do
      let Functor.Module {declarations} = modules Vector.! global
          Functor.Declarations {types} = declarations
          Functor.Annotated {meta} = types Vector.! local
          link :: Link.Type.Link 'Locality.Global -> Int -> ST s (Simple.Type Global)
          link (Link.Type.Global global local) id = do
            let Functor.Module {declarations} = modules Vector.! global
                Functor.Declarations {types} = declarations
                Functor.Annotated {content} = types Vector.! local
            TypeDeclaration {definition} <- content
            pure $ case definition of
              (Solved (TypeDefinition2.Types types)) TypeDefinition2.:::: _ -> types Strict.Vector.! id
              _ -> error "bad link lookup"
      annotation <- meta
      let context = globalBindings moduleSet
      -- todo, augment context with self to allow basic recursive inference
      TypeDeclaration.check context link annotation declaration

checkTypeDeclarationExtra ::
  forall s.
  Int ->
  Int ->
  Semantic.TypeDeclarationExtra.TypeDeclarationExtra Group Resolve Global ->
  Formula s (TypeDeclarationExtra Group Check Global)
checkTypeDeclarationExtra global local declaration = Formula7 {cycle, run}
  where
    cycle :: a
    cycle = cyclicalTypeChecking $ Semantic.TypeDeclarationExtra.position declaration
    run moduleSet@(Functor.ModuleSet modules) = do
      let Functor.Module {declarations} = modules Vector.! global
          Functor.Declarations {types} = declarations
          Functor.Annotated {content} = types Vector.! local
      proper <- content
      let context = globalBindings moduleSet
          link ::
            Link.Type.Link Locality.Global ->
            ST s (TypeDefinition2.Set Locality.Global Check Global)
          link (Link.Type.Global global local) = do
            let Functor.Module {declarations} = modules Vector.! global
                Functor.Declarations {types} = declarations
                Functor.Annotated {content} = types Vector.! local
            TypeDeclaration {definition} <- content
            case definition of
              _ TypeDefinition2.:::: set -> pure set
              _ -> error "bad link"
      proper <- Semantic.TypeDeclaration.ungroupM Link.Type.unglobal link proper
      extra <- TypeDeclarationExtra.check context (Type.Global global local) proper declaration
      Unify.runSolve $ TypeDeclarationExtra.solve extra

checkInstanceAnnotation ::
  p1 ->
  p2 ->
  Semantic.Instance.Instance Group Resolve Global ->
  Formula s (InstanceAnnotation Global)
checkInstanceAnnotation _ _ declaration = Formula7 {cycle, run}
  where
    cycle :: a
    cycle = cyclicalTypeChecking $ Semantic.Instance.startPosition declaration
    run modules = InstanceAnnotation.check (globalBindings modules) declaration

checkInstanceDeclaration ::
  Int ->
  Instance.Key.Key Global ->
  Semantic.Instance.Instance Group Resolve Global ->
  Formula s (Instance Group Check Global)
checkInstanceDeclaration global key declaration = Formula7 {cycle, run}
  where
    cycle :: a
    cycle = cyclicalTypeChecking $ Semantic.Instance.startPosition declaration
    run moduleSet@(Functor.ModuleSet modules) = do
      let Functor.Module {declarations} = modules Vector.! global
          Functor.Declarations {dataInstances, classInstances} = declarations
      case key of
        Instance.Key.Data {index, classKey} -> do
          let Functor.Annotated {meta} = dataInstances Vector.! index Map.! classKey
              key = Instance.Data {index1 = classKey, head1 = Type.Global global index}
          annotation <- meta
          instancex <- Instance.check (globalBindings moduleSet) key annotation declaration
          Unify.runSolve $ Instance.solve instancex
        Instance.Key.Class {index, dataKey} -> do
          let Functor.Annotated {meta} = classInstances Vector.! index Map.! dataKey
              key = Instance.Class {index2 = Type.Global global index, head2 = dataKey}
          annotation <- meta
          instancex <- Instance.check (globalBindings moduleSet) key annotation declaration
          Unify.runSolve $ Instance.solve instancex
