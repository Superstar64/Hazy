module Stage3.Tree.Module (Module (..), check) where

import Control.Monad.ST (ST)
import qualified Data.Map as Map
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Strict as Strict.Vector
import Error (cyclicalTypeChecking)
import Graph.Topological (Formula7 (..), Loeb7 (..), loeb7)
import qualified Graph.Topological7
import Stage1.Variable (toQualifiers)
import qualified Stage2.Index.Link.Term as Link.Term
import qualified Stage2.Index.Link.Type as Link.Type
import qualified Stage2.Index.Type as Type
import Stage2.Layout (Group)
import qualified Stage2.Locality as Locality
import Stage2.Scope (Global)
import Stage2.Stage (Check, Resolve)
import qualified Stage2.Tree.Combinators.Implicit as Implicit
import Stage2.Tree.Combinators.Inferred (Inferred (Solved))
import Stage2.Tree.Declaration (Declaration (..))
import qualified Stage2.Tree.Declaration as Declaration
import qualified Stage2.Tree.Declaration as Stage2 (Declaration)
import qualified Stage2.Tree.Declaration as Stage2.Declaration
import qualified Stage2.Tree.Declarations as Stage2.Declarations
import qualified Stage2.Tree.Definition4 as Definition4
import Stage2.Tree.Instance (Instance)
import qualified Stage2.Tree.Instance as Stage2.Instance
import Stage2.Tree.Module (Module (..))
import qualified Stage2.Tree.TypeDeclaration as Stage2.TypeDeclaration
import Stage2.Tree.TypeDeclarationExtra (TypeDeclarationExtra)
import qualified Stage2.Tree.TypeDeclarationExtra as Stage2.TypeDeclarationExtra
import qualified Stage2.Tree.TypeDefinition2 as TypeDefinition2
import Stage3.Check.Context (globalBindings)
import Stage3.Check.InstanceAnnotation (InstanceAnnotation)
import qualified Stage3.Check.InstanceAnnotation as InstanceAnnotation
import Stage3.Check.KindAnnotation (KindAnnotation)
import qualified Stage3.Check.KindAnnotation as KindAnnotation
import Stage3.Check.TypeAnnotation (TypeAnnotation)
import qualified Stage3.Check.TypeAnnotation as TypeAnnotation
import qualified Stage3.Functor.Annotated as Functor (Annotated (..))
import qualified Stage3.Functor.Declarations as Functor (Declarations (..))
import qualified Stage3.Functor.Instance.Key as Instance.Key
import Stage3.Functor.Module (fromStage2)
import qualified Stage3.Functor.Module as Functor (Module (..))
import Stage3.Functor.ModuleSet (mapWithKey)
import qualified Stage3.Functor.ModuleSet as Functor (ModuleSet (..))
import qualified Stage3.Simple.Scheme as Simple.Scheme
import qualified Stage3.Temporary.Declaration as Declaration.Unsolved
import qualified Stage3.Temporary.Instance as Instance (Key (..), check, solve)
import qualified Stage3.Temporary.TypeDeclarationExtra as TypeDeclarationExtra
import qualified Stage3.Tree.Declarations as Declarations
import Stage3.Tree.TypeDeclaration (TypeDeclaration (..))
import qualified Stage3.Tree.TypeDeclaration as TypeDeclaration
import qualified Stage3.Unify as Unify
import Stage4.Tree.Scheme (Scheme (..))
import qualified Stage4.Tree.SchemeOver as Scheme
import qualified Stage4.Tree.Type as Simple
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
  Stage2.Declaration locality Group Resolve Global ->
  Formula s (TypeAnnotation Global)
checkTermAnnotation _ _ declaration = Formula7 {cycle, run}
  where
    cycle :: a
    cycle = cyclicalTypeChecking $ Stage2.Declaration.position declaration
    run modules = TypeAnnotation.check (globalBindings modules) declaration

checkTermDeclaration ::
  forall s.
  Int ->
  Int ->
  Stage2.Declaration Locality.Global Group Resolve Global ->
  Formula s (Declaration Locality.Global Group Check Global)
checkTermDeclaration global local declaration = Formula7 {cycle, run}
  where
    cycle :: a
    cycle = cyclicalTypeChecking $ Stage2.Declaration.position declaration
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
              Definition4.Group (Implicit.Check set) -> Simple.Scheme.lift $ Scheme $ Scheme.map go set
                where
                  go = Scheme.Map $ \case
                    Definition4.Set set
                      | Definition4.Element {typex = Solved typex} <- set Strict.Vector.! id ->
                          typex
              _ -> error "bad link lookup"
      annotation <- meta
      let context = globalBindings moduleSet
      unsolved <- Declaration.Unsolved.check context link annotation declaration
      Declaration.Unsolved.solve unsolved

checkTypeAnnotation ::
  p1 ->
  p2 ->
  Stage2.TypeDeclaration.TypeDeclaration locality Group Resolve Global ->
  Formula s (KindAnnotation Global)
checkTypeAnnotation _ _ declaration = Formula7 {cycle, run}
  where
    cycle :: a
    cycle = cyclicalTypeChecking $ Stage2.TypeDeclaration.position declaration
    run modules = KindAnnotation.check (globalBindings modules) declaration

checkTypeDeclaration ::
  forall s.
  Int ->
  Int ->
  Stage2.TypeDeclaration.TypeDeclaration Locality.Global Group Resolve Global ->
  Formula s (TypeDeclaration Locality.Global Group Check Global)
checkTypeDeclaration global local declaration = Formula7 {cycle, run}
  where
    cycle :: a
    cycle = cyclicalTypeChecking $ Stage2.TypeDeclaration.position declaration
    run moduleSet@(Functor.ModuleSet modules) = do
      let self = Link.Type.Global global local
          Functor.Module {name, declarations} = modules Vector.! global
          Functor.Declarations {types} = declarations
          Functor.Annotated {meta} = types Vector.! local
          link :: Link.Type.Link 'Locality.Global -> Int -> ST s (Simple.Type Global)
          link (Link.Type.Global global local) id = do
            let Functor.Module {declarations} = modules Vector.! global
                Functor.Declarations {types} = declarations
                Functor.Annotated {content} = types Vector.! local
            TypeDeclaration {definition} <- content
            pure $ case definition of
              TypeDefinition2.Group (TypeDefinition2.Set set)
                | TypeDefinition2.Element {typex} <- set Strict.Vector.! id,
                  Solved typex <- typex ->
                    typex
              _ -> error "bad link lookup"
      annotation <- meta
      let context = globalBindings moduleSet
      -- todo, augment context with self to allow basic recursive inference
      TypeDeclaration.check context link (toQualifiers name) self annotation declaration

checkTypeDeclarationExtra ::
  forall s.
  Int ->
  Int ->
  Stage2.TypeDeclarationExtra.TypeDeclarationExtra Group Resolve Global ->
  Formula s (TypeDeclarationExtra Group Check Global)
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
          link ::
            Link.Type.Link Locality.Global ->
            ST s (TypeDefinition2.Set Locality.Global Check Global)
          link (Link.Type.Global global local) = do
            let Functor.Module {declarations} = modules Vector.! global
                Functor.Declarations {types} = declarations
                Functor.Annotated {content} = types Vector.! local
            TypeDeclaration {definition} <- content
            case definition of
              TypeDefinition2.Group set -> pure set
              _ -> error "bad link"
      proper <- Stage2.TypeDeclaration.ungroupM Link.Type.unglobal link proper
      extra <- TypeDeclarationExtra.check context (Type.Global global local) proper declaration
      TypeDeclarationExtra.solve extra

checkInstanceAnnotation ::
  p1 ->
  p2 ->
  Stage2.Instance.Instance Group Resolve Global ->
  Formula s (InstanceAnnotation Global)
checkInstanceAnnotation _ _ declaration = Formula7 {cycle, run}
  where
    cycle :: a
    cycle = cyclicalTypeChecking $ Stage2.Instance.startPosition declaration
    run modules = InstanceAnnotation.check (globalBindings modules) declaration

checkInstanceDeclaration ::
  Int ->
  Instance.Key.Key Global ->
  Stage2.Instance.Instance Group Resolve Global ->
  Formula s (Instance Group Check Global)
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
