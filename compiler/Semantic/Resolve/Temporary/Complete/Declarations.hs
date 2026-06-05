module Semantic.Resolve.Temporary.Complete.Declarations where

import Control.Monad.Fix (mfix)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import Error (orphanInstance, overlappingInstances)
import Order (orderNonEmpty, orderNonEmpty', orderWithInt)
import qualified Semantic.Index.Term as Term (Index)
import qualified Semantic.Index.Term0 as Term0 (Index (..))
import qualified Semantic.Index.Type0 as Type0
import qualified Semantic.Index.Type2 as Type2
import Semantic.Layout (Normal)
import Semantic.Resolve.Bindings (Bindings (Bindings))
import qualified Semantic.Resolve.Bindings as Bindings
import Semantic.Resolve.Context (Context (..))
import qualified Semantic.Resolve.Temporary.Complete.ClassInstance as ClassInstance
import Semantic.Resolve.Temporary.Complete.ConstructorDeclaration (ConstructorDeclaration)
import qualified Semantic.Resolve.Temporary.Complete.ConstructorDeclaration as Constructor (merge)
import qualified Semantic.Resolve.Temporary.Complete.ConstructorDeclaration as ConstructorDeclaration
import qualified Semantic.Resolve.Temporary.Complete.DataInstance as DataInstance
import Semantic.Resolve.Temporary.Complete.Declaration (Declaration (Declaration), name)
import qualified Semantic.Resolve.Temporary.Complete.Declaration as Term (bindings, indexes, merge, shrink)
import Semantic.Resolve.Temporary.Complete.TypeDeclaration (TypeDeclaration)
import qualified Semantic.Resolve.Temporary.Complete.TypeDeclaration as Type
  ( bindings,
    indexes,
    merge,
    shrink,
    shrinkExtra,
  )
import qualified Semantic.Resolve.Temporary.Partial.ConstructorDeclaration as Constructor (resolve)
import Semantic.Resolve.Temporary.Partial.Declaration (Key (Unnamed))
import qualified Semantic.Resolve.Temporary.Partial.Declaration as Term (resolve)
import qualified Semantic.Resolve.Temporary.Partial.TypeDeclaration as Declaration.Type (resolve)
import Semantic.Stage (Resolve)
import qualified Semantic.Tree.Declarations as Real
import Semantic.Tree.Instance (Instance)
import Syntax.Extensions (Extensions)
import Syntax.Position (Position)
import qualified Syntax.Tree.Declaration as Syntax (Declaration (..))
import Syntax.Tree.InstanceHead (InstanceHead (..))
import Syntax.Variable (ConstructorIdentifier, QualifiedConstructorIdentifier (..), Qualifiers (..))
import Verbose (Debug)

data Declarations scope = Declarations
  { terms :: !(Strict.Vector (Declaration scope)),
    constructors :: !(Strict.Vector ConstructorDeclaration),
    types :: !(Strict.Vector (TypeDeclaration scope)),
    dataInstances :: !(Vector (Map (Type2.Index scope) (Instance Normal Resolve scope))),
    classInstances :: !(Vector (Map (Type2.Index scope) (Instance Normal Resolve scope)))
  }

{-
Extensions are passed in manually rather then looking them up in the context
because resolve can't be strict on the context.

The context contains a map with all the symbols declarations have and since
map is strict it's keys, this would cause a cycle.
-}

resolve ::
  forall scope verbose.
  (Debug verbose) =>
  Context scope ->
  Extensions ->
  (Int -> Term.Index scope) ->
  [Syntax.Declaration Position] ->
  verbose (Declarations scope)
resolve context extensions share declarations = do
  types <- do
    let entries = foldMap (Declaration.Type.resolve context) declarations
    Strict.Vector.fromLazy <$> sequence (orderNonEmpty Type.merge entries)
  let typeIndexes = Type.indexes types
  constructors <- do
    let lookup index
          | Just index <- Map.lookup index typeIndexes =
              Just (index, types Strict.Vector.! index)
          | otherwise = Nothing
    pure $ orderNonEmpty' (Constructor.merge extensions) $ foldMap (Constructor.resolve lookup) declarations
  classInstances <- do
    let instances = foldMap (ClassInstance.resolve context lookup) declarations
        lookup name
          | Just index <- Map.lookup name typeIndexes = Just (index, types Strict.Vector.! index)
          | otherwise = Nothing
        ordered = orderWithInt (Map.unionWith (<>)) Map.empty (length types) (ClassInstance.prepare <$> instances)
        unique (instancex :| []) = ClassInstance.shrink instancex
        unique instances = overlappingInstances (ClassInstance.classPosition <$> toList instances)
    pure $ fmap (fmap unique) ordered
  dataInstances <- do
    let instances = foldMap (DataInstance.resolve context (`Map.lookup` typeIndexes)) declarations
        ordered = orderWithInt (Map.unionWith (<>)) Map.empty (length types) (DataInstance.prepare <$> instances)
        unique (instancex :| []) = DataInstance.shrink instancex
        unique instances = overlappingInstances (DataInstance.classPosition <$> toList instances)
    pure $ fmap (fmap unique) ordered
  terms <- mfix $ \terms -> do
    let entries = Term.resolve context lookupTerm lookupType lookupShared declarations
        lookupTerm name = terms Strict.Vector.! (termIndexes Map.! name)
        lookupType name = let index = typeIndexes Map.! name in (index, types Strict.Vector.! index)
        lookupShared temporary = shared Strict.Vector.! temporary
        shared = Strict.Vector.fromList $ do
          (Declaration {name = Unnamed _}, index) <- zip (toList terms) [0 ..]
          pure $ share index
        termIndexes = Term.indexes terms
    Strict.Vector.fromLazy <$> sequence (orderNonEmpty Term.merge entries)
  let noOrphans = foldr (seq . orphan (`Map.member` typeIndexes)) () declarations
  pure $
    seq
      noOrphans
      Declarations
        { terms,
          constructors,
          types,
          dataInstances,
          classInstances
        }

orphan :: (ConstructorIdentifier -> Bool) -> Syntax.Declaration Position -> ()
orphan local = \case
  Syntax.Instance {startPosition, className, instanceHead}
    | localClass -> ()
    | localData -> ()
    | otherwise -> orphanInstance startPosition
    where
      localClass
        | Local :=. classx <- className = local classx
        | otherwise = False
      localData
        | Head {typeName = Local :=. datax} <- instanceHead = local datax
        | otherwise = False
  _ -> ()

bindings ::
  (Monoid stability) =>
  (Int -> Term0.Index scope) ->
  (Int -> Type0.Index scope) ->
  Declarations scope ->
  Bindings stability scope
bindings
  termIndex
  typeIndex
  Declarations
    { terms,
      constructors,
      types
    } =
    Bindings
      { terms = Term.bindings termIndex typeIndex terms,
        constructors = ConstructorDeclaration.bindings typeIndex constructors,
        types = Type.bindings typeIndex types,
        stability = mempty
      }

shrink :: Declarations scope -> Real.Declarations locality Normal Resolve scope
shrink Declarations {terms, types, dataInstances, classInstances} =
  Real.Declarations
    { terms = Vector.catMaybes $ Term.shrink <$> Strict.Vector.toLazy terms,
      types = Type.shrink <$> Strict.Vector.toLazy types,
      typeExtras = Type.shrinkExtra <$> Strict.Vector.toLazy types,
      dataInstances,
      classInstances
    }
