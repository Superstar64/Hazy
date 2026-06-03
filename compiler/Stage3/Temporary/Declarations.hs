module Stage3.Temporary.Declarations (Declarations (..), check, solve) where

import Control.Monad.ST (ST)
import Data.Heptafunctor (heptamap)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Strict as Strict.Vector
import Error (cyclicalTypeChecking)
import Graph.Topological (Formula7 (..), loebST7)
import qualified Graph.Topological7
import Stage1.Variable (Qualifiers (Local))
import qualified Stage2.Index.Link.Term as Term
import qualified Stage2.Index.Link.Type as Link.Type
import qualified Stage2.Index.Type as Type
import qualified Stage2.Index.Type2 as Type2
import Stage2.Layout (Group)
import qualified Stage2.Locality as Locality
import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope
import Stage2.Stage (Check, Resolve)
import Stage2.Tree.Combinators.Inferred (Inferred (..))
import qualified Stage2.Tree.Declaration as Stage2 (Declaration)
import qualified Stage2.Tree.Declaration as Stage2.Declaration
import qualified Stage2.Tree.Declarations as Stage2 (Declarations (..))
import qualified Stage2.Tree.Instance as Stage2 (Instance)
import qualified Stage2.Tree.Instance as Stage2.Instance
import qualified Stage2.Tree.TypeDeclaration as Stage2 (TypeDeclaration)
import qualified Stage2.Tree.TypeDeclaration as Stage2.TypeDeclaration
import qualified Stage2.Tree.TypeDeclarationExtra as Stage2 (TypeDeclarationExtra)
import qualified Stage2.Tree.TypeDeclarationExtra as Stage2.TypeDeclarationExtra
import qualified Stage2.Tree.TypeDefinition2 as TypeDefinition2
import Stage3.Check.Context (Context (..), localBindings)
import Stage3.Check.InstanceAnnotation (InstanceAnnotation)
import qualified Stage3.Check.InstanceAnnotation as InstanceAnnotation
import Stage3.Check.KindAnnotation (KindAnnotation)
import qualified Stage3.Check.KindAnnotation as KindAnnotation
import Stage3.Check.TypeAnnotation (TypeAnnotation)
import qualified Stage3.Check.TypeAnnotation as TypeAnnotation
import qualified Stage3.Functor.Annotated as Functor (Annotated (..), content)
import Stage3.Functor.Declarations (mapWithKey)
import qualified Stage3.Functor.Declarations as Functor (Declarations (..), fromStage2)
import qualified Stage3.Functor.Instance.Key as Instance.Key
import Stage3.Temporary.Declaration (Declaration (..))
import qualified Stage3.Temporary.Declaration as Declaration
import qualified Stage3.Temporary.Definition4 as Definition4
import Stage3.Temporary.Instance (Instance)
import qualified Stage3.Temporary.Instance as Instance
import Stage3.Temporary.TypeDeclarationExtra (TypeDeclarationExtra)
import qualified Stage3.Temporary.TypeDeclarationExtra as TypeDeclarationExtra
import qualified Stage3.Tree.Declarations as Solved
import Stage3.Tree.TypeDeclaration (TypeDeclaration (..))
import qualified Stage3.Tree.TypeDeclaration as TypeDeclaration
import qualified Stage3.Unify as Unify
import qualified Stage4.Tree.Type as Simple
import Prelude hiding (Functor)

data Declarations locality s scope = Declarations
  { terms :: !(Vector (Declaration locality s scope)),
    types :: !(Vector (TypeDeclaration locality Group Check scope)),
    typeExtras :: !(Vector (TypeDeclarationExtra s scope)),
    classInstances :: !(Vector (Map (Type2.Index scope) (Instance s scope))),
    dataInstances :: !(Vector (Map (Type2.Index scope) (Instance s scope)))
  }

instance Unify.Zonk (Declarations locality) where
  zonk
    zonker
    Declarations
      { terms,
        types,
        typeExtras,
        classInstances,
        dataInstances
      } = do
      terms <- traverse (Unify.zonk zonker) terms
      typeExtras <- traverse (Unify.zonk zonker) typeExtras
      classInstances <- traverse (traverse (Unify.zonk zonker)) classInstances
      dataInstances <- traverse (traverse (Unify.zonk zonker)) dataInstances
      pure
        Declarations
          { terms,
            types,
            typeExtras,
            classInstances,
            dataInstances
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
  Stage2.Declarations Locality.Local Group Resolve (Scope.Declaration ':+ scope) ->
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
  Stage2.Declaration locality Group Resolve (Scope.Declaration ':+ scope) ->
  Formula s scope (TypeAnnotation (Scope.Declaration ':+ scope))
checkTermAnnotation context _ declaration = Formula7 {cycle, run}
  where
    cycle :: a
    cycle = cyclicalTypeChecking $ Stage2.Declaration.position declaration
    run declarations = do
      context <- pure $ localBindings declarations context
      TypeAnnotation.check context declaration

checkTermDeclaration ::
  forall s scope.
  Context s scope ->
  Int ->
  Stage2.Declaration Locality.Local Group Resolve (Scope.Declaration ':+ scope) ->
  Formula s scope (Declaration Locality.Local s (Scope.Declaration ':+ scope))
checkTermDeclaration context index declaration = Formula7 {cycle, run}
  where
    cycle :: a
    cycle = cyclicalTypeChecking $ Stage2.Declaration.position declaration
    run declarations@Functor.Declarations {terms} = do
      context <- pure $ localBindings declarations context
      let Functor.Annotated {meta} = terms Vector.! index
          link :: Term.Link Locality.Local -> Int -> ST s (Unify.Scheme s (Scope.Declaration ':+ scope))
          link (Term.Declaration local) id = do
            let Functor.Annotated {content} = terms Vector.! local
            Declaration {definition} <- content
            pure $ case definition of
              Definition4.Group set -> Unify.Scheme $ Unify.mapScheme go set
                where
                  go = Unify.MapScheme $ \case
                    Definition4.Set set
                      | Definition4.Element {typex} <- set Strict.Vector.! id -> typex
              _ -> error "bad link lookup"
      annotation <- meta
      Declaration.check context link annotation declaration

checkTypeAnnotation ::
  Context s scope ->
  p ->
  Stage2.TypeDeclaration locality Group Resolve (Scope.Declaration ':+ scope) ->
  Formula s scope (KindAnnotation (Scope.Declaration ':+ scope))
checkTypeAnnotation context _ declaration = Formula7 {cycle, run}
  where
    cycle :: a
    cycle = cyclicalTypeChecking $ Stage2.TypeDeclaration.position declaration
    run declarations = do
      context <- pure $ localBindings declarations context
      KindAnnotation.check context declaration

checkTypeDeclaration ::
  forall s scope.
  Context s scope ->
  Int ->
  Stage2.TypeDeclaration Locality.Local Group Resolve (Scope.Declaration ':+ scope) ->
  Formula s scope (TypeDeclaration Locality.Local Group Check (Scope.Declaration ':+ scope))
checkTypeDeclaration context index declaration = Formula7 {cycle, run}
  where
    cycle :: a
    cycle = cyclicalTypeChecking $ Stage2.TypeDeclaration.position declaration
    run declarations@Functor.Declarations {types} = do
      context <- pure $ localBindings declarations context
      let Functor.Annotated {meta} = types Vector.! index
          link :: Link.Type.Link Locality.Local -> Int -> ST s (Simple.Type (Scope.Declaration ':+ scope))
          link (Link.Type.Declaration local) id = do
            let Functor.Annotated {content} = types Vector.! local
            TypeDeclaration {definition} <- content
            pure $ case definition of
              TypeDefinition2.Group (TypeDefinition2.Set set)
                | TypeDefinition2.Element {typex} <- set Strict.Vector.! id,
                  Solved typex <- typex ->
                    typex
              _ -> error "bad link lookup"
      annotation <- meta
      TypeDeclaration.check context link annotation declaration

checkTypeDeclarationExtra ::
  forall s scope.
  Context s scope ->
  Int ->
  Stage2.TypeDeclarationExtra Group Resolve (Scope.Declaration ':+ scope) ->
  Formula s scope (TypeDeclarationExtra s (Scope.Declaration ':+ scope))
checkTypeDeclarationExtra context index declaration = Formula7 {cycle, run}
  where
    cycle :: a
    cycle = cyclicalTypeChecking $ Stage2.TypeDeclarationExtra.position declaration
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
              TypeDefinition2.Group set -> pure set
              _ -> error "bad link"
      proper <- Stage2.TypeDeclaration.ungroupM Link.Type.unlocal link proper
      TypeDeclarationExtra.check context (Type.Declaration index) proper declaration

checkInstanceAnnotation ::
  Context s scope ->
  p ->
  Stage2.Instance.Instance Group Resolve (Scope.Declaration ':+ scope) ->
  Formula s scope (InstanceAnnotation (Scope.Declaration ':+ scope))
checkInstanceAnnotation context _ declaration = Formula7 {cycle, run}
  where
    cycle :: a
    cycle = cyclicalTypeChecking $ Stage2.Instance.startPosition declaration
    run declarations = InstanceAnnotation.check (localBindings declarations context) declaration

checkInstanceDeclaration ::
  Context s scope ->
  Instance.Key.Key (Scope.Declaration ':+ scope) ->
  Stage2.Instance Group Resolve (Scope.Declaration ':+ scope) ->
  Formula s scope (Instance s (Scope.Declaration ':+ scope))
checkInstanceDeclaration context key declaration = Formula7 {cycle, run}
  where
    cycle :: a
    cycle = cyclicalTypeChecking $ Stage2.Instance.startPosition declaration
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

solve :: Declarations locality s scope -> ST s (Solved.Declarations locality Group Check scope)
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
