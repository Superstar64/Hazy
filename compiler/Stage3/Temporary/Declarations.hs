module Stage3.Temporary.Declarations where

import Control.Monad.ST (ST)
import Data.Acyclic (loebST6)
import Data.Hexafunctor (Hexafunctor (hexamap))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Error (cyclicalTypeChecking)
import Stage1.Variable (Qualifiers (Local))
import qualified Stage2.Index.Term0 as Term0
import qualified Stage2.Index.Type as Type
import qualified Stage2.Index.Type2 as Type2
import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope
import qualified Stage2.Tree.Declarations as Stage2 (Declarations (..))
import qualified Stage2.Tree.Instance as Stage2.Instance
import qualified Stage2.Tree.TermDeclaration as Stage2.TermDeclaration
import qualified Stage2.Tree.TypeDeclaration as Stage2.TypeDeclaration
import Stage3.Check.Context (Context (..), localBindings)
import qualified Stage3.Check.InstanceAnnotation as InstanceAnnotation
import qualified Stage3.Check.KindAnnotation as KindAnnotation
import qualified Stage3.Check.TypeAnnotation as TypeAnnotation
import qualified Stage3.Functor.Annotated as Functor (Annotated (..), content)
import Stage3.Functor.Declarations (mapWithKey)
import qualified Stage3.Functor.Declarations as Functor (Declarations (..), fromStage2)
import qualified Stage3.Functor.Instance.Key as Instance.Key
import Stage3.Temporary.TermDeclaration (TermDeclaration)
import qualified Stage3.Temporary.TermDeclaration as TermDeclaration
import qualified Stage3.Tree.Declarations as Solved
import Stage3.Tree.Instance (Instance)
import qualified Stage3.Tree.Instance as Instance
import Stage3.Tree.TypeDeclaration (TypeDeclaration)
import qualified Stage3.Tree.TypeDeclaration as TypeDeclaration
import qualified Stage3.Unify as Unify

data Declarations s scope = Declarations
  { terms :: !(Vector (TermDeclaration s scope)),
    types :: !(Vector (TypeDeclaration scope)),
    classInstances :: !(Vector (Map (Type2.Index scope) (Instance scope))),
    dataInstances :: !(Vector (Map (Type2.Index scope) (Instance scope)))
  }

fromFunctor ::
  Functor.Declarations
    scope
    a
    (TermDeclaration s scope)
    c
    (TypeDeclaration scope)
    e
    (Instance scope) ->
  Declarations s scope
fromFunctor Functor.Declarations {terms, types, classInstances, dataInstances} =
  Declarations
    { terms = Functor.content <$> terms,
      types = Functor.content <$> types,
      dataInstances = fmap (fmap Functor.content) dataInstances,
      classInstances = fmap (fmap Functor.content) classInstances
    }

check ::
  forall s scope.
  Context s scope ->
  Stage2.Declarations (Scope.Declaration ':+ scope) ->
  ST
    s
    ( Context s (Scope.Declaration ':+ scope),
      Declarations s (Scope.Declaration ':+ scope)
    )
check context declarations = do
  functor <-
    loebST6 $
      let go1 index declaration =
            ( cyclicalTypeChecking $ Stage2.TermDeclaration.position declaration,
              \declarations -> do
                context <- pure $ localBindings declarations context
                typex <- Unify.fresh Unify.typex
                TypeAnnotation.check (Term0.Declaration index) typex context declaration
            )
          go2 index declaration =
            ( cyclicalTypeChecking $ Stage2.TermDeclaration.position declaration,
              \declarations@Functor.Declarations {terms} -> do
                context <- pure $ localBindings declarations context
                let Functor.Annotated {meta} = terms Vector.! index
                annotation <- meta
                TermDeclaration.check context annotation declaration
            )
          go3 _ declaration =
            ( cyclicalTypeChecking $ Stage2.TypeDeclaration.position declaration,
              \declarations -> do
                context <- pure $ localBindings declarations context
                KindAnnotation.check context declaration
            )
          go4 index declaration =
            ( cyclicalTypeChecking $ Stage2.TypeDeclaration.position declaration,
              \declarations@Functor.Declarations {types} -> do
                context <- pure $ localBindings declarations context
                let Functor.Annotated {meta} = types Vector.! index
                annotation <- meta
                TypeDeclaration.check context annotation declaration
            )
          go5 _ declaration =
            ( cyclicalTypeChecking $ Stage2.Instance.startPosition declaration,
              \declarations -> InstanceAnnotation.check (localBindings declarations context) declaration
            )
          go6 key declaration =
            ( cyclicalTypeChecking $ Stage2.Instance.startPosition declaration,
              \declarations -> do
                let Functor.Declarations {dataInstances, classInstances} = declarations
                case key of
                  Instance.Key.Data {index, classKey} -> do
                    let Functor.Annotated {meta} = dataInstances Vector.! index Map.! classKey
                        dataKey = Type2.Index $ Type.Declaration index
                    annotation <- meta
                    Instance.check (localBindings declarations context) classKey dataKey annotation declaration
                  Instance.Key.Class {index, dataKey} -> do
                    let Functor.Annotated {meta} = classInstances Vector.! index Map.! dataKey
                        classKey = Type2.Index $ Type.Declaration index
                    annotation <- meta
                    Instance.check (localBindings declarations context) classKey dataKey annotation declaration
            )
       in mapWithKey go1 go2 go3 go4 go5 go6 $ Functor.fromStage2 Local declarations
  pure (localBindings (hexamap pure (const ()) pure pure pure pure functor) context, fromFunctor functor)

solve :: Declarations s scope -> ST s (Solved.Declarations scope)
solve Declarations {terms, types, dataInstances, classInstances} = do
  terms <- traverse TermDeclaration.solve terms
  pure
    Solved.Declarations
      { terms,
        types,
        dataInstances,
        classInstances
      }
