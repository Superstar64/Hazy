module Stage3.Tree.Module where

import Data.Acyclic (Loeb7 (..), loeb7)
import qualified Data.Map as Map
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Error (cyclicalTypeChecking)
import Stage1.Variable (FullQualifiers)
import qualified Stage2.Index.Term0 as Term0
import qualified Stage2.Index.Type as Type
import qualified Stage2.Index.Type2 as Type2
import Stage2.Scope (Global)
import qualified Stage2.Tree.Instance as Stage2.Instance
import qualified Stage2.Tree.Module as Stage2 (Module (..))
import qualified Stage2.Tree.TermDeclaration as Stage2.TermDeclaration
import qualified Stage2.Tree.TypeDeclaration as Stage2.TypeDeclaration
import Stage3.Check.Context (globalBindings)
import qualified Stage3.Check.InstanceAnnotation as InstanceAnnotation
import qualified Stage3.Check.KindAnnotation as KindAnnotation
import qualified Stage3.Check.TypeAnnotation as TypeAnnotation
import qualified Stage3.Functor.Annotated as Functor (Annotated (..))
import qualified Stage3.Functor.Declarations as Functor (Declarations (..))
import qualified Stage3.Functor.Instance.Key as Instance.Key
import Stage3.Functor.Module (fromStage2)
import qualified Stage3.Functor.Module as Functor (Module (..))
import Stage3.Functor.ModuleSet (mapWithKey)
import qualified Stage3.Functor.ModuleSet as Functor (ModuleSet (..))
import qualified Stage3.Temporary.TermDeclaration as TermDeclaration.Unsolved
import Stage3.Tree.Declarations (Declarations)
import qualified Stage3.Tree.Declarations as Declarations
import Stage3.Tree.Instance (Instance)
import qualified Stage3.Tree.Instance as Instance
import Stage3.Tree.TermDeclaration (TermDeclaration)
import Stage3.Tree.TypeDeclaration (TypeDeclaration)
import qualified Stage3.Tree.TypeDeclaration as TypeDeclaration
import Stage3.Tree.TypeDeclarationExtra (TypeDeclarationExtra)
import qualified Stage3.Tree.TypeDeclarationExtra as TypeDeclarationExtra

data Module = Module
  { name :: !FullQualifiers,
    declarations :: Declarations Global
  }
  deriving (Show)

fromFunctor ::
  Functor.Module
    a
    (TermDeclaration Global)
    b
    (TypeDeclaration Global)
    (TypeDeclarationExtra Global)
    e
    (Instance Global) ->
  Module
fromFunctor (Functor.Module {name, declarations}) =
  Module
    { name,
      declarations = Declarations.fromFunctor declarations
    }

fromFunctors ::
  Functor.ModuleSet
    a
    (TermDeclaration Global)
    b
    (TypeDeclaration Global)
    (TypeDeclarationExtra Global)
    e
    (Instance Global) ->
  Vector Module
fromFunctors (Functor.ModuleSet modules) = fmap fromFunctor modules

check :: Vector Stage2.Module -> Vector Module
check modules =
  fromFunctors $
    loeb7 $
      Loeb7 $
        let go1 global local declaration =
              ( cyclicalTypeChecking $ Stage2.TermDeclaration.position declaration,
                \modules ->
                  TypeAnnotation.check (Term0.Global global local) () (globalBindings modules) declaration
              )
            go2 global local declaration =
              ( cyclicalTypeChecking $ Stage2.TermDeclaration.position declaration,
                \moduleSet@(Functor.ModuleSet modules) -> do
                  let Functor.Module {declarations} = modules Vector.! global
                      Functor.Declarations {terms} = declarations
                      Functor.Annotated {meta} = terms Vector.! local
                  annotation <- meta
                  let context = globalBindings moduleSet
                  -- todo, augment context with self to allow basic recursive inference
                  annotation <- TypeAnnotation.instanciate annotation
                  unsolved <- TermDeclaration.Unsolved.check context annotation declaration
                  TermDeclaration.Unsolved.solve unsolved
              )
            go3 _ _ declaration =
              ( cyclicalTypeChecking $ Stage2.TypeDeclaration.position declaration,
                \modules -> KindAnnotation.check (globalBindings modules) declaration
              )
            go4 global local declaration =
              ( cyclicalTypeChecking $ Stage2.TypeDeclaration.position declaration,
                \moduleSet@(Functor.ModuleSet modules) -> do
                  let Functor.Module {declarations} = modules Vector.! global
                      Functor.Declarations {types} = declarations
                      Functor.Annotated {meta} = types Vector.! local
                  annotation <- meta
                  let context = globalBindings moduleSet
                  -- todo, augment context with self to allow basic recursive inference
                  TypeDeclaration.check context annotation declaration
              )
            go5 global local declaration =
              ( cyclicalTypeChecking $ Stage2.TypeDeclaration.position declaration,
                \moduleSet@(Functor.ModuleSet modules) -> do
                  let Functor.Module {declarations} = modules Vector.! global
                      Functor.Declarations {types} = declarations
                      Functor.Annotated {content} = types Vector.! local
                  proper <- content
                  let context = globalBindings moduleSet
                  TypeDeclarationExtra.check context proper declaration
              )
            go6 _ _ declaration =
              ( cyclicalTypeChecking $ Stage2.Instance.startPosition declaration,
                \modules -> InstanceAnnotation.check (globalBindings modules) declaration
              )
            go7 global key declaration =
              ( cyclicalTypeChecking $ Stage2.Instance.startPosition declaration,
                \moduleSet@(Functor.ModuleSet modules) -> do
                  let Functor.Module {declarations} = modules Vector.! global
                      Functor.Declarations {dataInstances, classInstances} = declarations
                  case key of
                    Instance.Key.Data {index, classKey} -> do
                      let Functor.Annotated {meta} = dataInstances Vector.! index Map.! classKey
                          dataKey = Type2.Index $ Type.Global global index
                      annotation <- meta
                      Instance.check (globalBindings moduleSet) classKey dataKey annotation declaration
                    Instance.Key.Class {index, dataKey} -> do
                      let Functor.Annotated {meta} = classInstances Vector.! index Map.! dataKey
                          classKey = Type2.Index $ Type.Global global index
                      annotation <- meta
                      Instance.check (globalBindings moduleSet) classKey dataKey annotation declaration
              )
         in mapWithKey go1 go2 go3 go4 go5 go6 go7 (Functor.ModuleSet $ fmap fromStage2 modules)
