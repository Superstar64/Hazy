module Stage2.Temporary.Complete.Declarations where

import Control.Monad.Fix (mfix)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Strict as Strict (Vector)
import qualified Data.Vector.Strict as Strict.Vector
import Error (overlappingInstances)
import Order (orderNonEmpty, orderNonEmpty', orderWithInt)
import Stage1.Extensions (Extensions)
import Stage1.Position (Position)
import qualified Stage1.Tree.Declaration as Stage1 (Declaration (..))
import qualified Stage2.Index.Term0 as Term0 (Index (..))
import qualified Stage2.Index.Type as Type (Index (..))
import qualified Stage2.Index.Type2 as Type2
import Stage2.Resolve.Bindings (Bindings (Bindings))
import qualified Stage2.Resolve.Bindings as Bindings
import Stage2.Resolve.Context (Context (..))
import qualified Stage2.Temporary.Complete.ClassInstance as ClassInstance
import Stage2.Temporary.Complete.ConstructorDeclaration (ConstructorDeclaration)
import qualified Stage2.Temporary.Complete.ConstructorDeclaration as Constructor (merge)
import qualified Stage2.Temporary.Complete.ConstructorDeclaration as ConstructorDeclaration
import qualified Stage2.Temporary.Complete.DataInstance as DataInstance
import Stage2.Temporary.Complete.Shared (Shared)
import qualified Stage2.Temporary.Complete.Shared as Shared (resolve, shrink)
import Stage2.Temporary.Complete.TermDeclaration (TermDeclaration)
import qualified Stage2.Temporary.Complete.TermDeclaration as Term (bindings, indexes, merge, shrink)
import Stage2.Temporary.Complete.TypeDeclaration (TypeDeclaration)
import qualified Stage2.Temporary.Complete.TypeDeclaration as Type (bindings, indexes, merge, shrink)
import qualified Stage2.Temporary.Partial.ConstructorDeclaration as Constructor (resolve)
import qualified Stage2.Temporary.Partial.TermDeclaration as Term (resolve)
import qualified Stage2.Temporary.Partial.TypeDeclaration as Declaration.Type (resolve)
import qualified Stage2.Tree.Declarations as Real
import Stage2.Tree.Instance (Instance)
import Verbose (Debug)

data Declarations scope = Declarations
  { terms :: !(Strict.Vector (TermDeclaration scope)),
    constructors :: !(Strict.Vector ConstructorDeclaration),
    types :: !(Strict.Vector (TypeDeclaration scope)),
    shared :: !(Strict.Vector (Shared scope)),
    dataInstances :: !(Vector (Map (Type2.Index scope) (Instance scope))),
    classInstances :: !(Vector (Map (Type2.Index scope) (Instance scope)))
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
  [Stage1.Declaration Position] ->
  verbose (Declarations scope)
resolve context extensions declarations = do
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
  let shared = Strict.Vector.fromList (Shared.resolve context declarations)
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
    let instances = foldMap (DataInstance.resolve context resolve) declarations
        resolve =
          DataInstance.Resolve
            { DataInstance.typeIndexes
            }
        ordered = orderWithInt (Map.unionWith (<>)) Map.empty (length types) (DataInstance.prepare <$> instances)
        unique (instancex :| []) = DataInstance.shrink instancex
        unique instances = overlappingInstances (DataInstance.classPosition <$> toList instances)
    pure $ fmap (fmap unique) ordered

  terms <- mfix $ \terms -> do
    let entries = Term.resolve context lookupTerm lookupType lookupShared 0 declarations
        lookupTerm name = terms Strict.Vector.! (termIndexes Map.! name)
        lookupType name = let index = typeIndexes Map.! name in (index, types Strict.Vector.! index)
        lookupShared index = shared Strict.Vector.! index
        termIndexes = Term.indexes terms
    Strict.Vector.fromLazy <$> sequence (orderNonEmpty Term.merge entries)

  pure
    Declarations
      { terms,
        constructors,
        types,
        shared,
        dataInstances,
        classInstances
      }

bindings ::
  (Monoid stability) =>
  (Int -> Term0.Index scope) ->
  (Int -> Type.Index scope) ->
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
      { Bindings.terms = Term.bindings termIndex typeIndex terms,
        Bindings.constructors = ConstructorDeclaration.bindings typeIndex constructors,
        Bindings.types = Type.bindings typeIndex types,
        Bindings.stability = mempty
      }

shrink :: Declarations scope -> Real.Declarations scope
shrink Declarations {terms, types, shared, dataInstances, classInstances} =
  Real.Declarations
    { Real.terms = Vector.catMaybes $ Term.shrink <$> Strict.Vector.toLazy terms,
      Real.types = Type.shrink <$> Strict.Vector.toLazy types,
      Real.shared = Shared.shrink <$> Strict.Vector.toLazy shared,
      Real.dataInstances,
      Real.classInstances
    }
