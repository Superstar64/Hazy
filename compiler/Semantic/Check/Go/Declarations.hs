module Semantic.Check.Go.Declarations where

import Control.Monad.ST (ST)
import Data.Functor.Compose (Compose (..))
import Data.Functor.Identity (Identity (..))
import Data.Functor2 (fmap2)
import qualified Data.Map as Map
import Data.NaturalTransformation (NaturalTransformation (..))
import qualified Data.Vector as Vector
import Data.Void (Void)
import Error (cyclicalTypeChecking)
import Graph.Topological (loebST)
import qualified Graph.Topological as Topological
import Semantic.Check.Context (Context (..), localBindings)
import qualified Semantic.Check.Go.Declaration as Declaration
import Semantic.Check.Go.Definition4 (Solve (Solve))
import qualified Semantic.Check.Go.Definition4 as Definition4
import qualified Semantic.Check.Go.Instance as Instance
import qualified Semantic.Check.Go.TypeDeclaration as TypeDeclaration (check)
import qualified Semantic.Check.Go.TypeDeclarationExtra as TypeDeclarationExtra (check)
import Semantic.Functor2 (Proper (..), traverse2)
import qualified Semantic.Index.Link.Term as Term
import qualified Semantic.Index.Link.Term as Term.Link
import qualified Semantic.Index.Link.Type as Link.Type
import qualified Semantic.Index.Link.Type as Type
import qualified Semantic.Index.Link.Type as Type.Link
import qualified Semantic.Index.Type0 as Type0
import qualified Semantic.Index.Type2 as Type2
import Semantic.Layout (Group)
import qualified Semantic.Locality as Locality
import Semantic.Scope (Environment (..))
import qualified Semantic.Scope as Scope
import Semantic.Stage (Check, Resolve)
import Semantic.Tree.Declaration (Declaration)
import Semantic.Tree.Declarations (Declarations (..), Local (..))
import qualified Semantic.Tree.Declarations as Semantic (Local (..))
import Semantic.Tree.Instance (Instance)
import Semantic.Tree.TypeDeclaration (TypeDeclaration (..))
import qualified Semantic.Tree.TypeDeclaration as TypeDeclaration (ungroupM)
import qualified Semantic.Tree.TypeDeclarationExtra as TypeDeclarationExtra (TypeDeclarationExtra (..))
import qualified Semantic.Tree.TypeDefinition2 as TypeDefinition2
import qualified Semantic.Tree.TypeGroup as TypeGroup
import qualified Semantic.Unify as Unify
import Prelude hiding (Functor)

checkImpl ::
  (Monad solve) =>
  (Type.Link locality -> Type0.Index scope) ->
  (Int -> Term.Link locality) ->
  (Int -> Type.Link locality) ->
  Definition4.Solve solve logical s scope ->
  ( declarations (ST s) ->
    Term.Link locality ->
    Declaration solve' logical locality (ST s) Group Check scope
  ) ->
  ( declarations (ST s) ->
    Type.Link locality ->
    TypeDeclaration locality (ST s) Group Check scope
  ) ->
  ( declarations (ST s) ->
    Int ->
    Type2.Index scope ->
    Instance solve'' (ST s) Group Check scope
  ) ->
  ( declarations (ST s) ->
    Int ->
    Type2.Index scope ->
    Instance solve'' (ST s) Group Check scope
  ) ->
  (declarations (ST s) -> Context s scope) ->
  Declarations Identity Void locality Identity Group Resolve scope ->
  Declarations solve logical locality (Topological.Formula declarations s) Group Check scope
checkImpl
  typeIndexLink
  termLink
  typeLink
  solve@Solve {solve = run}
  reflection1
  reflection2
  reflection3
  reflection4
  information
  Declarations {terms, types, typeExtras, dataInstances, classInstances} =
    Declarations
      { terms =
          let check index = Declaration.check (termLink index) solve reflection1 information
           in Vector.imap check terms,
        types =
          let check index = TypeDeclaration.check (typeLink index) reflection2 information
           in Vector.imap check types,
        typeExtras =
          let check index (Identity (Identity typeExtra)) =
                Topological.Formula
                  { cycle = cyclicalTypeChecking $ TypeDeclarationExtra.position typeExtra,
                    run = \declarations -> do
                      let context = information declarations
                          typeDeclaration = reflection2 declarations (typeLink index)
                          lookup index = case reflection2 declarations index of
                            TypeDeclaration {definition = TypeDefinition2.Group group} -> do
                              _ TypeGroup.:::: set <- group
                              pure set
                            _ -> error "bad lookup"
                      typeDeclaration <- traverse2 (Morph $ Compose . fmap Identity) typeDeclaration
                      typeDeclaration <- TypeDeclaration.ungroupM typeIndexLink lookup typeDeclaration
                      declaration <-
                        TypeDeclarationExtra.check
                          context
                          (typeIndex index)
                          typeDeclaration
                          typeExtra
                      run declaration
                  }
           in Vector.imap check typeExtras,
        dataInstances =
          let check head index instancex =
                Instance.check key solve reflection information instancex
                where
                  key = Instance.Data {head1 = typeIndex head, index1 = index}
                  reflection declarations = reflection3 declarations head index
           in Vector.imap (Map.mapWithKey . check) dataInstances,
        classInstances =
          let check index head instancex =
                Instance.check key solve reflection information instancex
                where
                  key = Instance.Class {head2 = head, index2 = typeIndex index}
                  reflection declarations = reflection4 declarations index head
           in Vector.imap (Map.mapWithKey . check) classInstances
      }
    where
      typeIndex = Type0.normal . typeIndexLink . typeLink

data Result s scope
  = !(Context s (Scope.Declaration ':+ scope))
      :&
      !(Unify.Solve s (Semantic.Local Group Check scope))

infix 5 :&

lookupTerm ::
  Proper (Declarations solve logical Locality.Local) layout stage scope loeb ->
  Term.Link Locality.Local ->
  Declaration solve logical Locality.Local loeb layout stage scope
lookupTerm (Proper Declarations {terms}) (Term.Link.Declaration index) = terms Vector.! index

lookupType ::
  Proper (Declarations solve logical Locality.Local) layout stage scope loeb ->
  Type.Link Locality.Local ->
  TypeDeclaration Locality.Local loeb layout stage scope
lookupType (Proper Declarations {types}) (Type.Link.Declaration index) = types Vector.! index

lookupDataInstance ::
  Proper (Declarations solve logical locality) layout stage scope loeb ->
  Int ->
  Type2.Index scope ->
  Instance solve loeb layout stage scope
lookupDataInstance (Proper Declarations {dataInstances}) local index = dataInstances Vector.! local Map.! index

lookupClassInstance ::
  Proper (Declarations solve logical locality) layout stage scope loeb ->
  Int ->
  Type2.Index scope ->
  Instance solve loeb layout stage scope
lookupClassInstance (Proper Declarations {classInstances}) local index = classInstances Vector.! local Map.! index

check :: Context s scope -> Semantic.Local Group Resolve scope -> ST s (Result s scope)
check context (Semantic.Local declarations) = do
  let locals (Proper declarations) = localBindings context declarations
  Proper declarations <-
    loebST $
      Proper $
        checkImpl
          Link.Type.unlocal
          Term.Declaration
          Link.Type.Declaration
          Definition4.delay
          lookupTerm
          lookupType
          lookupDataInstance
          lookupClassInstance
          locals
          declarations
  let context = locals (fmap2 (Morph $ pure . runIdentity) $ Proper declarations)
  declarations <- pure $ solve declarations
  pure $ context :& (Local <$> declarations)

solve ::
  Declarations
    (Unify.Solve s)
    (Unify.Logical s (Scope.Declaration ':+ scope))
    Locality.Local
    Identity
    Group
    Check
    (Scope.Declaration ':+ scope) ->
  Unify.Solve
    s
    ( Declarations
        Identity
        Void
        Locality.Local
        Identity
        Group
        Check
        (Scope.Declaration ':+ scope)
    )
solve Declarations {terms, types, typeExtras, dataInstances, classInstances} =
  declarations
    <$> traverse Declaration.solve terms
    <*> traverse (traverse (fmap Identity)) typeExtras
    <*> traverse (traverse Instance.solve) dataInstances
    <*> traverse (traverse Instance.solve) classInstances
  where
    declarations terms typeExtras dataInstances classInstances =
      Declarations
        { terms,
          types,
          typeExtras,
          dataInstances,
          classInstances
        }
