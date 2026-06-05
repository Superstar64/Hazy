module Semantic.Check.Temporary.Declarations (Declarations (..), check, solve) where

import Control.Monad.ST (ST)
import qualified Core.Tree.Type as Simple
import Data.Heptafunctor (heptamap)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Strict as Strict.Vector
import Error (cyclicalTypeChecking)
import Graph.Topological (Formula7 (..), loebST7)
import qualified Graph.Topological7
import Semantic.Check.Context (Context (..), localBindings)
import qualified Semantic.Check.Functor.Annotated as Functor (Annotated (..), content)
import Semantic.Check.Functor.Declarations (mapWithKey)
import qualified Semantic.Check.Functor.Declarations as Functor (Declarations (..), fromStage2)
import qualified Semantic.Check.Functor.Instance.Key as Instance.Key
import qualified Semantic.Check.Go.Declarations as Solved
import Semantic.Check.Go.TypeDeclaration (TypeDeclaration (..))
import qualified Semantic.Check.Go.TypeDeclaration as TypeDeclaration
import Semantic.Check.InstanceAnnotation (InstanceAnnotation)
import qualified Semantic.Check.InstanceAnnotation as InstanceAnnotation
import Semantic.Check.KindAnnotation (KindAnnotation)
import qualified Semantic.Check.KindAnnotation as KindAnnotation
import Semantic.Check.Temporary.Declaration (Declaration (..))
import qualified Semantic.Check.Temporary.Declaration as Declaration
import qualified Semantic.Check.Temporary.Definition4 as Definition4
import Semantic.Check.Temporary.Instance (Instance)
import qualified Semantic.Check.Temporary.Instance as Instance
import Semantic.Check.Temporary.TypeDeclarationExtra (TypeDeclarationExtra)
import qualified Semantic.Check.Temporary.TypeDeclarationExtra as TypeDeclarationExtra
import Semantic.Check.TypeAnnotation (TypeAnnotation)
import qualified Semantic.Check.TypeAnnotation as TypeAnnotation
import qualified Semantic.Index.Link.Term as Term
import qualified Semantic.Index.Link.Type as Link.Type
import qualified Semantic.Index.Type as Type
import qualified Semantic.Index.Type2 as Type2
import Semantic.Layout (Group)
import qualified Semantic.Locality as Locality
import Semantic.Scope (Environment (..))
import qualified Semantic.Scope as Scope
import Semantic.Stage (Check, Resolve)
import Semantic.Tree.Combinators.Inferred (Inferred (..))
import qualified Semantic.Tree.Declaration as Semantic (Declaration)
import qualified Semantic.Tree.Declaration as Semantic.Declaration
import qualified Semantic.Tree.Declarations as Semantic (Declarations (..))
import qualified Semantic.Tree.Instance as Semantic (Instance)
import qualified Semantic.Tree.Instance as Semantic.Instance
import qualified Semantic.Tree.TypeDeclaration as Semantic (TypeDeclaration)
import qualified Semantic.Tree.TypeDeclaration as Semantic.TypeDeclaration
import qualified Semantic.Tree.TypeDeclarationExtra as Semantic (TypeDeclarationExtra)
import qualified Semantic.Tree.TypeDeclarationExtra as Semantic.TypeDeclarationExtra
import qualified Semantic.Tree.TypeDefinition2 as TypeDefinition2
import qualified Semantic.Unify as Unify
import Syntax.Variable (Qualifiers (Local))
import Prelude hiding (Functor)

data Declarations locality s scope = Declarations
  { terms :: !(Vector (Declaration locality s scope)),
    types :: !(Vector (TypeDeclaration locality Group Check scope)),
    typeExtras :: !(Vector (TypeDeclarationExtra s scope)),
    classInstances :: !(Vector (Map (Type2.Index scope) (Instance s scope))),
    dataInstances :: !(Vector (Map (Type2.Index scope) (Instance s scope)))
  }

type Formula s scope z =
  Formula7
    (Functor.Declarations (Scope.Declaration ':+ scope))
    s
    (TypeAnnotation (Scope.Declaration ':+ scope))
    (Declaration Locality.Local s (Scope.Declaration ':+ scope))
    (KindAnnotation (Scope.Declaration ':+ scope))
    (TypeDeclaration Locality.Local Group Check (Scope.Declaration ':+ scope))
    (TypeDeclarationExtra s (Scope.Declaration ':+ scope))
    (InstanceAnnotation (Scope.Declaration ':+ scope))
    (Instance s (Scope.Declaration ':+ scope))
    z

fromFunctor ::
  Functor.Declarations
    scope
    a
    (Declaration locality s scope)
    b
    (TypeDeclaration locality Group Check scope)
    (TypeDeclarationExtra s scope)
    c
    (Instance s scope) ->
  Declarations locality s scope
fromFunctor
  Functor.Declarations
    { terms,
      types,
      typeExtras,
      classInstances,
      dataInstances
    } =
    Declarations
      { terms = Functor.content <$> terms,
        types = Functor.content <$> types,
        typeExtras,
        dataInstances = fmap (fmap Functor.content) dataInstances,
        classInstances = fmap (fmap Functor.content) classInstances
      }

check ::
  Context s scope ->
  Semantic.Declarations Locality.Local Group Resolve (Scope.Declaration ':+ scope) ->
  ST
    s
    ( Context s (Scope.Declaration ':+ scope),
      Declarations Locality.Local s (Scope.Declaration ':+ scope)
    )
check context declarations = do
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
      $ Functor.fromStage2 Local declarations
  let lifted = heptamap pure pure pure pure pure pure (const ()) functor
  pure (localBindings lifted context, fromFunctor functor)

checkTermAnnotation ::
  Context s scope ->
  p ->
  Semantic.Declaration locality Group Resolve (Scope.Declaration ':+ scope) ->
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
  Semantic.Declaration Locality.Local Group Resolve (Scope.Declaration ':+ scope) ->
  Formula s scope (Declaration Locality.Local s (Scope.Declaration ':+ scope))
checkTermDeclaration context index declaration = Formula7 {cycle, run}
  where
    cycle :: a
    cycle = cyclicalTypeChecking $ Semantic.Declaration.position declaration
    run declarations@Functor.Declarations {terms} = do
      context <- pure $ localBindings declarations context
      let Functor.Annotated {meta} = terms Vector.! index
          link :: Term.Link Locality.Local -> Int -> ST s (Unify.Scheme s (Scope.Declaration ':+ scope))
          link (Term.Declaration local) id = do
            let Functor.Annotated {content} = terms Vector.! local
            Declaration {definition} <- content
            pure $ case definition of
              types Definition4.:::: _ -> Unify.Scheme $ Unify.mapScheme go types
                where
                  go = Unify.MapScheme $ \case
                    Definition4.Types types -> types Strict.Vector.! id
              _ -> error "bad link lookup"
      annotation <- meta
      Declaration.check context link annotation declaration

checkTypeAnnotation ::
  Context s scope ->
  p ->
  Semantic.TypeDeclaration locality Group Resolve (Scope.Declaration ':+ scope) ->
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
  Semantic.TypeDeclaration Locality.Local Group Resolve (Scope.Declaration ':+ scope) ->
  Formula s scope (TypeDeclaration Locality.Local Group Check (Scope.Declaration ':+ scope))
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
              Solved (TypeDefinition2.Types types) TypeDefinition2.:::: _ -> types Strict.Vector.! id
              _ -> error "bad link lookup"
      annotation <- meta
      TypeDeclaration.check context link annotation declaration

checkTypeDeclarationExtra ::
  forall s scope.
  Context s scope ->
  Int ->
  Semantic.TypeDeclarationExtra Group Resolve (Scope.Declaration ':+ scope) ->
  Formula s scope (TypeDeclarationExtra s (Scope.Declaration ':+ scope))
checkTypeDeclarationExtra context index declaration = Formula7 {cycle, run}
  where
    cycle :: a
    cycle = cyclicalTypeChecking $ Semantic.TypeDeclarationExtra.position declaration
    run declarations@Functor.Declarations {types} = do
      context <- pure $ localBindings declarations context
      let Functor.Annotated {content} = types Vector.! index
      proper <- content
      let link ::
            Link.Type.Link Locality.Local ->
            ST s (TypeDefinition2.Set Locality.Local Check (Scope.Declaration ':+ scope))
          link (Link.Type.Declaration local) = do
            let Functor.Annotated {content} = types Vector.! local
            TypeDeclaration {definition} <- content
            case definition of
              _ TypeDefinition2.:::: set -> pure set
              _ -> error "bad link"
      proper <- Semantic.TypeDeclaration.ungroupM Link.Type.unlocal link proper
      TypeDeclarationExtra.check context (Type.Declaration index) proper declaration

checkInstanceAnnotation ::
  Context s scope ->
  p ->
  Semantic.Instance.Instance Group Resolve (Scope.Declaration ':+ scope) ->
  Formula s scope (InstanceAnnotation (Scope.Declaration ':+ scope))
checkInstanceAnnotation context _ declaration = Formula7 {cycle, run}
  where
    cycle :: a
    cycle = cyclicalTypeChecking $ Semantic.Instance.startPosition declaration
    run declarations = InstanceAnnotation.check (localBindings declarations context) declaration

checkInstanceDeclaration ::
  Context s scope ->
  Instance.Key.Key (Scope.Declaration ':+ scope) ->
  Semantic.Instance Group Resolve (Scope.Declaration ':+ scope) ->
  Formula s scope (Instance s (Scope.Declaration ':+ scope))
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

solve :: Declarations locality s scope -> Unify.Solve s (Solved.Declarations locality Group Check scope)
solve
  Declarations
    { terms,
      types,
      typeExtras,
      dataInstances,
      classInstances
    } = do
    terms <- traverse Declaration.solve terms
    typeExtras <- traverse TypeDeclarationExtra.solve typeExtras
    dataInstances <- traverse (traverse Instance.solve) dataInstances
    classInstances <- traverse (traverse Instance.solve) classInstances
    pure
      Solved.Declarations
        { terms,
          types,
          typeExtras,
          dataInstances,
          classInstances
        }
