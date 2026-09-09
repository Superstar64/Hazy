module Semantic.Check.Go.Declarations (Declarations (..), Result (..), fromFunctor, check, solve) where

import Control.Monad.ST (ST)
import qualified Core.Tree.Type as Simple
import Data.Functor.Identity (Identity (..))
import Data.Heptafunctor (heptamap)
import qualified Data.Map as Map
import qualified Data.Vector as Vector
import qualified Data.Vector.Strict as Strict.Vector
import Data.Void (Void)
import Error (cyclicalTypeChecking)
import Graph.Topological (Formula7 (..), loebST7)
import qualified Graph.Topological7
import Semantic.Check.Context (Context (..), localBindings)
import qualified Semantic.Check.Functor.Annotated as Functor (Annotated (..), content)
import Semantic.Check.Functor.Declarations (mapWithKey)
import qualified Semantic.Check.Functor.Declarations as Functor (Declarations (..), fromStage2)
import qualified Semantic.Check.Functor.Instance.Key as Instance.Key
import qualified Semantic.Check.Go.Declaration as Declaration
import qualified Semantic.Check.Go.Instance as Instance
import Semantic.Check.Go.TypeDeclaration (TypeDeclaration (..))
import qualified Semantic.Check.Go.TypeDeclaration as TypeDeclaration
import qualified Semantic.Check.Go.TypeDeclarationExtra as TypeDeclarationExtra
import Semantic.Check.InstanceAnnotation (InstanceAnnotation)
import qualified Semantic.Check.InstanceAnnotation as InstanceAnnotation
import Semantic.Check.KindAnnotation (KindAnnotation)
import qualified Semantic.Check.KindAnnotation as KindAnnotation
import Semantic.Check.TypeAnnotation (TypeAnnotation)
import qualified Semantic.Check.TypeAnnotation as TypeAnnotation
import qualified Semantic.Index.Link.Term as Term
import qualified Semantic.Index.Link.Type as Link.Type
import qualified Semantic.Index.Type as Type
import Semantic.Layout (Group)
import qualified Semantic.Locality as Locality
import Semantic.Scope (Environment (..))
import qualified Semantic.Scope as Scope
import Semantic.Stage (Check, Resolve)
import Semantic.Tree.Combinators.Inferred (Inferred (..))
import Semantic.Tree.Declaration (Declaration (..))
import qualified Semantic.Tree.Declaration as Semantic (Declaration)
import qualified Semantic.Tree.Declaration as Semantic.Declaration
import Semantic.Tree.Declarations (Declarations (..))
import qualified Semantic.Tree.Declarations as Semantic (Local (..))
import qualified Semantic.Tree.Definition4 as Definition4
import qualified Semantic.Tree.Group as Group
import Semantic.Tree.Instance (Instance)
import qualified Semantic.Tree.Instance as Semantic (Instance (..))
import qualified Semantic.Tree.Instance as Semantic.Instance
import qualified Semantic.Tree.InstanceDefinition2 as InstanceDefinition
import qualified Semantic.Tree.TypeDeclaration as Semantic (TypeDeclaration)
import qualified Semantic.Tree.TypeDeclaration as Semantic.TypeDeclaration
import Semantic.Tree.TypeDeclarationExtra (TypeDeclarationExtra)
import qualified Semantic.Tree.TypeDeclarationExtra as Semantic (TypeDeclarationExtra)
import qualified Semantic.Tree.TypeDeclarationExtra as Semantic.TypeDeclarationExtra
import qualified Semantic.Tree.TypeDefinition2 as TypeDefinition2
import qualified Semantic.Tree.TypeGroup as TypeGroup
import qualified Semantic.Unify as Unify
import qualified Syntax.Variable as Variable
import Prelude hiding (Functor)

type Formula s scope z =
  Formula7
    (Functor.Declarations (Scope.Declaration ':+ scope))
    s
    (TypeAnnotation (Scope.Declaration ':+ scope))
    ( Declaration
        (Unify.Solve s)
        (Unify.Logical s (Scope.Declaration ':+ scope))
        Locality.Local
        Identity
        Group
        Check
        (Scope.Declaration ':+ scope)
    )
    (KindAnnotation (Scope.Declaration ':+ scope))
    (TypeDeclaration Locality.Local Identity Group Check (Scope.Declaration ':+ scope))
    (Identity (Unify.Solve s (TypeDeclarationExtra Group Check (Scope.Declaration ':+ scope))))
    (InstanceAnnotation (Scope.Declaration ':+ scope))
    (Semantic.Instance (Unify.Solve s) Identity Group Check (Scope.Declaration ':+ scope))
    z

fromFunctor ::
  Functor.Declarations
    scope
    a1
    (Declaration solve logical locality loeb layout stage scope)
    a2
    (TypeDeclaration locality loeb layout stage scope)
    (loeb (solve (TypeDeclarationExtra layout stage scope)))
    a3
    (Instance solve loeb layout stage scope) ->
  Declarations solve logical locality loeb layout stage scope
fromFunctor (Functor.Declarations {terms, types, typeExtras, dataInstances, classInstances}) =
  Declarations
    { terms = Functor.content <$> terms,
      types = Functor.content <$> types,
      typeExtras,
      dataInstances = fmap (fmap Functor.content) dataInstances,
      classInstances = fmap (fmap Functor.content) classInstances
    }

data Result s scope
  = !(Context s (Scope.Declaration ':+ scope))
      :&
      !(Unify.Solve s (Semantic.Local Group Check scope))

infix 5 :&

check :: Context s scope -> Semantic.Local Group Resolve scope -> ST s (Result s scope)
check context (Semantic.Local declarations) = do
  functor <-
    loebST7
      $ mapWithKey
        (checkTermAnnotation context)
        (checkTermDeclaration context)
        (checkTypeAnnotation context)
        (checkTypeDeclaration context)
        (checkTypeDeclarationExtra context)
        (checkInstanceAnnotation context)
        (checkInstanceDeclaration context)
      $ Functor.fromStage2 Variable.Local declarations
  let lifted = heptamap pure pure pure pure pure pure (const ()) functor
  pure $ localBindings lifted context :& (fmap Semantic.Local $ solve $ fromFunctor functor)

checkTermAnnotation ::
  Context s scope ->
  p ->
  Semantic.Declaration Identity Void locality Identity Group Resolve (Scope.Declaration ':+ scope) ->
  Formula s scope (TypeAnnotation (Scope.Declaration ':+ scope))
checkTermAnnotation context _ declaration = Formula7 {cycle, run}
  where
    cycle :: a
    cycle = cyclicalTypeChecking $ Semantic.Declaration.position declaration
    run declarations = do
      context <- pure $ localBindings declarations context
      TypeAnnotation.check context declaration

checkTermDeclaration ::
  forall s scope.
  Context s scope ->
  Int ->
  Semantic.Declaration Identity Void Locality.Local Identity Group Resolve (Scope.Declaration ':+ scope) ->
  Formula
    s
    scope
    ( Declaration
        (Unify.Solve s)
        (Unify.Logical s (Scope.Declaration ':+ scope))
        Locality.Local
        Identity
        Group
        Check
        (Scope.Declaration ':+ scope)
    )
checkTermDeclaration context index declaration = Formula7 {cycle, run}
  where
    cycle :: a
    cycle = cyclicalTypeChecking $ Semantic.Declaration.position declaration
    run declarations@Functor.Declarations {terms} = do
      context <- pure $ localBindings declarations context
      let Functor.Annotated {meta} = terms Vector.! index
          link :: Term.Link Locality.Local -> Int -> ST s (Unify.Forall s (Scope.Declaration ':+ scope))
          link (Term.Declaration local) id = do
            let Functor.Annotated {content} = terms Vector.! local
            Declaration {definition} <- content
            pure $ case definition of
              Definition4.Group (Identity (Solved types Group.:::: _)) -> Unify.mapForall go types
                where
                  go = Unify.MapForall $ \case
                    Group.Types types -> types Strict.Vector.! id
              _ -> error "bad link lookup"
      annotation <- meta
      Declaration.check context link annotation declaration

checkTypeAnnotation ::
  Context s scope ->
  p ->
  Semantic.TypeDeclaration locality Identity Group Resolve (Scope.Declaration ':+ scope) ->
  Formula s scope (KindAnnotation (Scope.Declaration ':+ scope))
checkTypeAnnotation context _ declaration = Formula7 {cycle, run}
  where
    cycle :: a
    cycle = cyclicalTypeChecking $ Semantic.TypeDeclaration.position declaration
    run declarations = do
      context <- pure $ localBindings declarations context
      KindAnnotation.check context declaration

checkTypeDeclaration ::
  forall s scope.
  Context s scope ->
  Int ->
  Semantic.TypeDeclaration Locality.Local Identity Group Resolve (Scope.Declaration ':+ scope) ->
  Formula s scope (TypeDeclaration Locality.Local Identity Group Check (Scope.Declaration ':+ scope))
checkTypeDeclaration context index declaration = Formula7 {cycle, run}
  where
    cycle :: a
    cycle = cyclicalTypeChecking $ Semantic.TypeDeclaration.position declaration
    run declarations@Functor.Declarations {types} = do
      context <- pure $ localBindings declarations context
      let Functor.Annotated {meta} = types Vector.! index
          link :: Link.Type.Link Locality.Local -> Int -> ST s (Simple.Type (Scope.Declaration ':+ scope))
          link (Link.Type.Declaration local) id = do
            let Functor.Annotated {content} = types Vector.! local
            TypeDeclaration {definition} <- content
            pure $ case definition of
              TypeDefinition2.Group (Identity (Solved (TypeGroup.Types types) TypeGroup.:::: _)) ->
                types Strict.Vector.! id
              _ -> error "bad link lookup"
      annotation <- meta
      TypeDeclaration.check context link annotation declaration

checkTypeDeclarationExtra ::
  forall s scope.
  Context s scope ->
  Int ->
  Identity (Identity (Semantic.TypeDeclarationExtra Group Resolve (Scope.Declaration ':+ scope))) ->
  Formula s scope (Identity (Unify.Solve s (TypeDeclarationExtra Group Check (Scope.Declaration ':+ scope))))
checkTypeDeclarationExtra context index (Identity (Identity declaration)) = Formula7 {cycle, run}
  where
    cycle :: a
    cycle = cyclicalTypeChecking $ Semantic.TypeDeclarationExtra.position declaration
    run declarations@Functor.Declarations {types} = do
      context <- pure $ localBindings declarations context
      let Functor.Annotated {content} = types Vector.! index
      proper <- content
      let link ::
            Link.Type.Link Locality.Local ->
            ST s (TypeGroup.Set Locality.Local Check (Scope.Declaration ':+ scope))
          link (Link.Type.Declaration local) = do
            let Functor.Annotated {content} = types Vector.! local
            TypeDeclaration {definition} <- content
            case definition of
              TypeDefinition2.Group (Identity (_ TypeGroup.:::: set)) -> pure set
              _ -> error "bad link"
      proper <- Semantic.TypeDeclaration.ungroupM Link.Type.unlocal link proper
      Identity <$> TypeDeclarationExtra.check context (Type.Declaration index) proper declaration

checkInstanceAnnotation ::
  Context s scope ->
  p ->
  Semantic.Instance.Instance Identity Identity Group Resolve (Scope.Declaration ':+ scope) ->
  Formula s scope (InstanceAnnotation (Scope.Declaration ':+ scope))
checkInstanceAnnotation context _ declaration = Formula7 {cycle, run}
  where
    cycle :: a
    cycle = cyclicalTypeChecking $ Semantic.Instance.startPosition declaration
    run declarations = InstanceAnnotation.check (localBindings declarations context) annotation
    Semantic.Instance {definition = Identity annotation InstanceDefinition.::: _} = declaration

checkInstanceDeclaration ::
  Context s scope ->
  Instance.Key.Key (Scope.Declaration ':+ scope) ->
  Semantic.Instance Identity Identity Group Resolve (Scope.Declaration ':+ scope) ->
  Formula s scope (Semantic.Instance (Unify.Solve s) Identity Group Check (Scope.Declaration ':+ scope))
checkInstanceDeclaration context key declaration = Formula7 {cycle, run}
  where
    cycle :: a
    cycle = cyclicalTypeChecking $ Semantic.Instance.startPosition declaration
    run declarations = do
      let Functor.Declarations {dataInstances, classInstances} = declarations
      case key of
        Instance.Key.Data {index, classKey} -> do
          let Functor.Annotated {meta} = dataInstances Vector.! index Map.! classKey
              key = Instance.Data {index1 = classKey, head1 = Type.Declaration index}
          annotation <- meta
          Instance.check (localBindings declarations context) key annotation declaration
        Instance.Key.Class {index, dataKey} -> do
          let Functor.Annotated {meta} = classInstances Vector.! index Map.! dataKey
              key = Instance.Class {index2 = Type.Declaration index, head2 = dataKey}
          annotation <- meta
          Instance.check (localBindings declarations context) key annotation declaration

solve ::
  Declarations (Unify.Solve s) (Unify.Logical s scope) locality Identity Group Check scope ->
  Unify.Solve s (Declarations Identity Void locality Identity Group Check scope)
solve
  Declarations
    { terms,
      types,
      typeExtras,
      dataInstances,
      classInstances
    } = do
    terms <- traverse Declaration.solve terms
    typeExtras <- traverse runIdentity typeExtras
    dataInstances <- traverse (traverse Instance.solve) dataInstances
    classInstances <- traverse (traverse Instance.solve) classInstances
    pure
      Declarations
        { terms,
          types,
          typeExtras = fmap (Identity . Identity) typeExtras,
          dataInstances,
          classInstances
        }
