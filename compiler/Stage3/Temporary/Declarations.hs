module Stage3.Temporary.Declarations (Declarations (..), check, solve) where

import Control.Monad.ST (ST)
import Data.Acyclic (loebST8)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Octafunctor (octamap)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Error (cyclicalTypeChecking)
import Stage1.Variable (Qualifiers (Local))
import qualified Stage2.Index.Type as Type
import qualified Stage2.Index.Type2 as Type2
import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope
import qualified Stage2.Tree.Declarations as Stage2 (Declarations (..))
import qualified Stage2.Tree.Instance as Stage2 (Instance)
import qualified Stage2.Tree.Instance as Stage2.Instance
import qualified Stage2.Tree.Shared as Stage2 (Shared (..))
import qualified Stage2.Tree.Shared as Stage2.Shared
import qualified Stage2.Tree.TermDeclaration as Stage2 (TermDeclaration)
import qualified Stage2.Tree.TermDeclaration as Stage2.TermDeclaration
import qualified Stage2.Tree.TypeDeclaration as Stage2 (TypeDeclaration)
import qualified Stage2.Tree.TypeDeclaration as Stage2.TypeDeclaration
import Stage3.Check.Context (Context (..), localBindings)
import Stage3.Check.InstanceAnnotation (InstanceAnnotation)
import qualified Stage3.Check.InstanceAnnotation as InstanceAnnotation
import Stage3.Check.KindAnnotation (KindAnnotation)
import qualified Stage3.Check.KindAnnotation as KindAnnotation
import Stage3.Check.TypeAnnotation (LocalTypeAnnotation)
import qualified Stage3.Check.TypeAnnotation as TypeAnnotation
import qualified Stage3.Functor.Annotated as Functor (Annotated (..), content)
import Stage3.Functor.Declarations (mapWithKey)
import qualified Stage3.Functor.Declarations as Functor (Declarations (..), fromStage2)
import qualified Stage3.Functor.Instance.Key as Instance.Key
import Stage3.Temporary.Shared (Shared (..))
import qualified Stage3.Temporary.Shared as Shared
import Stage3.Temporary.TermDeclaration (TermDeclaration)
import qualified Stage3.Temporary.TermDeclaration as TermDeclaration
import qualified Stage3.Tree.Declarations as Solved
import Stage3.Tree.Instance (Instance)
import qualified Stage3.Tree.Instance as Instance
import qualified Stage3.Tree.TermDeclaration as Solved.TermDeclaration
import Stage3.Tree.TypeDeclaration (TypeDeclaration)
import qualified Stage3.Tree.TypeDeclaration as Solved.TypeDeclaration
import qualified Stage3.Tree.TypeDeclaration as TypeDeclaration
import Stage3.Tree.TypeDeclarationExtra (TypeDeclarationExtra)
import qualified Stage3.Tree.TypeDeclarationExtra as TypeDeclarationExtra
import qualified Stage3.Unify as Unify
import Prelude hiding (Functor)

data Declarations s scope = Declarations
  { terms :: !(Vector (TermDeclaration s scope)),
    types :: !(Vector (TypeDeclaration scope)),
    shared :: !(Vector (Shared s scope)),
    typeExtras :: !(Vector (TypeDeclarationExtra scope)),
    classInstances :: !(Vector (Map (Type2.Index scope) (Instance scope))),
    dataInstances :: !(Vector (Map (Type2.Index scope) (Instance scope)))
  }

instance Unify.Zonk Declarations where
  zonk
    zonker
    Declarations
      { terms,
        types,
        shared,
        typeExtras,
        classInstances,
        dataInstances
      } = do
      terms <- traverse (Unify.zonk zonker) terms
      shared <- traverse (Unify.zonk zonker) shared
      pure
        Declarations
          { terms,
            shared,
            types,
            typeExtras,
            classInstances,
            dataInstances
          }

type Functor s scope =
  Functor.Declarations
    (Scope.Declaration ':+ scope)
    (ST s (LocalTypeAnnotation s (Scope.Declaration ':+ scope)))
    (ST s (TermDeclaration s (Scope.Declaration ':+ scope)))
    (ST s (Shared s (Scope.Declaration ':+ scope)))
    (ST s (KindAnnotation (Scope.Declaration ':+ scope)))
    (ST s (TypeDeclaration (Scope.Declaration ':+ scope)))
    (ST s (TypeDeclarationExtra (Scope.Declaration ':+ scope)))
    (ST s (InstanceAnnotation (Scope.Declaration ':+ scope)))
    (ST s (Instance (Scope.Declaration ':+ scope)))

fromFunctor ::
  Functor.Declarations
    scope
    a
    (TermDeclaration s scope)
    (Shared s scope)
    c
    (TypeDeclaration scope)
    (TypeDeclarationExtra scope)
    e
    (Instance scope) ->
  Declarations s scope
fromFunctor
  Functor.Declarations
    { terms,
      types,
      shared,
      typeExtras,
      classInstances,
      dataInstances
    } =
    Declarations
      { terms = Functor.content <$> terms,
        types = Functor.content <$> types,
        shared,
        typeExtras,
        dataInstances = fmap (fmap Functor.content) dataInstances,
        classInstances = fmap (fmap Functor.content) classInstances
      }

check ::
  Context s scope ->
  Stage2.Declarations (Scope.Declaration ':+ scope) ->
  ST
    s
    ( Context s (Scope.Declaration ':+ scope),
      Declarations s (Scope.Declaration ':+ scope)
    )
check context declarations = do
  functor <-
    loebST8
      $ mapWithKey
        (checkTermAnnotation context)
        (checkTermDeclaration context)
        (checkShared context)
        (checkTypeAnnotation context)
        (checkTypeDeclaration context)
        (checkTypeDeclarationExtra context)
        (checkInstanceAnnotation context)
        (checkInstanceDeclaration context)
      $ Functor.fromStage2 Local declarations
  let lifted =
        octamap
          pure
          (const ())
          (const ())
          pure
          pure
          pure
          pure
          (const ())
          functor
  pure (localBindings lifted context, fromFunctor functor)

checkTermAnnotation ::
  Context s scope ->
  p ->
  Stage2.TermDeclaration (Scope.Declaration ':+ scope) ->
  ( a,
    Functor s scope ->
    ST s (LocalTypeAnnotation s (Scope.Declaration ':+ scope))
  )
checkTermAnnotation context _ declaration =
  ( cyclicalTypeChecking $ Stage2.TermDeclaration.position declaration,
    \declarations -> do
      context <- pure $ localBindings declarations context
      TypeAnnotation.checkLocal context declaration
  )

checkTermDeclaration ::
  Context s scope ->
  Int ->
  Stage2.TermDeclaration (Scope.Declaration ':+ scope) ->
  ( a,
    Functor s scope -> ST s (TermDeclaration s (Scope.Declaration ':+ scope))
  )
checkTermDeclaration context index declaration =
  ( cyclicalTypeChecking $ Stage2.TermDeclaration.position declaration,
    \declarations@Functor.Declarations {terms, shared} -> do
      context <- pure $ localBindings declarations context
      let Functor.Annotated {meta} = terms Vector.! index
      annotation <- meta
      let any = TypeAnnotation.local annotation
      let share index = do
            Shared {body} <- shared Vector.! index
            pure $ Unify.Scheme $ Unify.mapScheme (Unify.MapScheme Shared.typex) body
      TermDeclaration.check context share any declaration
  )

checkShared ::
  Context s scope ->
  p ->
  Stage2.Shared (Scope.Declaration ':+ scope) ->
  ( a,
    Functor s scope ->
    ST s (Shared s (Scope.Declaration ':+ scope))
  )
checkShared context _ declaration =
  ( cyclicalTypeChecking $ Stage2.Shared.equalPosition declaration,
    \declarations -> do
      context <- pure $ localBindings declarations context
      typex <- Unify.fresh Unify.typex
      Shared.check context (Just typex) declaration
  )

checkTypeAnnotation ::
  Context s scope ->
  p ->
  Stage2.TypeDeclaration (Scope.Declaration ':+ scope) ->
  ( a,
    Functor s scope ->
    ST s (KindAnnotation (Scope.Declaration ':+ scope))
  )
checkTypeAnnotation context _ declaration =
  ( cyclicalTypeChecking $ Stage2.TypeDeclaration.position declaration,
    \declarations -> do
      context <- pure $ localBindings declarations context
      KindAnnotation.check context declaration
  )

checkTypeDeclaration ::
  Context s scope ->
  Int ->
  Stage2.TypeDeclaration (Scope.Declaration ':+ scope) ->
  ( a,
    Functor s scope ->
    ST s (TypeDeclaration (Scope.Declaration ':+ scope))
  )
checkTypeDeclaration context index declaration =
  ( cyclicalTypeChecking $ Stage2.TypeDeclaration.position declaration,
    \declarations@Functor.Declarations {types} -> do
      context <- pure $ localBindings declarations context
      let Functor.Annotated {meta} = types Vector.! index
      annotation <- meta
      TypeDeclaration.check context annotation declaration
  )

checkTypeDeclarationExtra ::
  Context s scope ->
  Int ->
  Stage2.TypeDeclaration (Scope.Declaration ':+ scope) ->
  ( a,
    Functor s scope ->
    ST s (TypeDeclarationExtra (Scope.Declaration ':+ scope))
  )
checkTypeDeclarationExtra context index declaration =
  ( cyclicalTypeChecking $ Stage2.TypeDeclaration.position declaration,
    \declarations@Functor.Declarations {types} -> do
      context <- pure $ localBindings declarations context
      let Functor.Annotated {content} = types Vector.! index
      proper <- content
      TypeDeclarationExtra.check context (Type.Declaration index) proper declaration
  )

checkInstanceAnnotation ::
  Context s scope ->
  p ->
  Stage2.Instance.Instance (Scope.Declaration ':+ scope) ->
  ( a,
    Functor s scope ->
    ST s (InstanceAnnotation (Scope.Declaration ':+ scope))
  )
checkInstanceAnnotation context _ declaration =
  ( cyclicalTypeChecking $ Stage2.Instance.startPosition declaration,
    \declarations -> InstanceAnnotation.check (localBindings declarations context) declaration
  )

checkInstanceDeclaration ::
  Context s scope ->
  Instance.Key.Key (Scope.Declaration ':+ scope) ->
  Stage2.Instance (Scope.Declaration ':+ scope) ->
  ( a,
    Functor s scope ->
    ST s (Instance (Scope.Declaration ':+ scope))
  )
checkInstanceDeclaration context key declaration =
  ( cyclicalTypeChecking $ Stage2.Instance.startPosition declaration,
    \declarations -> do
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
  )

solve :: Declarations s scope -> ST s (Solved.Declarations scope)
solve
  Declarations
    { terms,
      types,
      shared,
      typeExtras,
      dataInstances,
      classInstances
    } = do
    terms <- traverse TermDeclaration.solve terms
    shared <- traverse Shared.solve shared
    pure
      Solved.Declarations
        { terms = Solved.TermDeclaration.strict <$> terms,
          types = Solved.TypeDeclaration.strict <$> types,
          shared,
          typeExtras,
          dataInstances,
          classInstances
        }
