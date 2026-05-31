{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage2.Tree.Module
  ( Module (..),
    labelContext,
    resolve,
    connect,
    seperate,
  )
where

import Data.Maybe (fromJust)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Error (Position)
import Graph.StronglyConnected (Index (..), tarjan)
import qualified Stage1.Tree.Module as Stage1 (Module)
import Stage1.Variable (FullQualifiers, toQualifiers)
import qualified Stage2.Group.Functor.Term.Declarations as Functor.Term (indexes)
import qualified Stage2.Group.Functor.Term.ModuleSet as Functor.Term (ModuleSet (..), (!))
import qualified Stage2.Group.Functor.Type.Declarations as Functor.Type (indexes)
import qualified Stage2.Group.Functor.Type.ModuleSet as Functor.Type (ModuleSet (..), (!))
import qualified Stage2.Index.Link.Term as Term
import qualified Stage2.Index.Link.Type as Type
import qualified Stage2.Index.Table.Local as Local.Table
import qualified Stage2.Index.Table.Term as Table.Term
import qualified Stage2.Index.Table.Type as Table.Type
import qualified Stage2.Label.Context as Label (Context (Context))
import qualified Stage2.Label.Context as Label.Context
import Stage2.Layout (Group, Normal)
import qualified Stage2.Locality as Locality
import Stage2.Scope (Global)
import qualified Stage2.Scope as Scope
import Stage2.Stage (Check, Resolve)
import {-# SOURCE #-} qualified Stage2.Temporary.Complete.Module as Complete
import Stage2.Tree.Combinators.Implicit (Implicit)
import Stage2.Tree.Declaration (Declaration (Declaration))
import qualified Stage2.Tree.Declaration as Declaration
import Stage2.Tree.Declarations (Declarations (..))
import qualified Stage2.Tree.Declarations as Declarations
import qualified Stage2.Tree.Definition4 as Definition4
import Stage2.Tree.TypeDeclaration (TypeDeclaration (..))
import qualified Stage2.Tree.TypeDeclaration as TypeDeclaration
import qualified Stage2.Tree.TypeDefinition2 as TypeDefinition2
import Verbose (Debug)

data Module layout stage = Module
  { name :: !FullQualifiers,
    declarations :: Declarations Locality.Global layout stage Global
  }
  deriving (Show)

labelContext :: Vector (Module Normal Resolve) -> Label.Context Global
labelContext modules =
  Label.Context
    { terms = Table.Term.Global $ labelTerms modules,
      locals = Local.Table.Global,
      types = Table.Type.Global $ labelTypes modules
    }
  where
    labelTerms = fmap $ \Module {name, declarations = Declarations {terms}} ->
      Declaration.labelBinding (toQualifiers name) <$> terms
    labelTypes = fmap $ \Module {name, declarations = Declarations {types}} ->
      TypeDeclaration.labelBinding (toQualifiers name) <$> types

resolve :: (Debug verbose) => Vector (Stage1.Module Position) -> verbose (Vector (Module Normal Resolve))
resolve modules = fmap Complete.shrink <$> Complete.resolve modules

connect :: Vector (Module Normal Resolve) -> Vector (Module Group Resolve)
connect modules = Vector.imap go modules
  where
    go index Module {name, declarations} =
      Module
        { name,
          declarations =
            Declarations.group
              (toQualifiers name)
              Term.global
              Type.global
              indexTerm'
              indexType'
              termGroup
              typeGroup
              declarations
        }
      where
        termGroup = termGroups Vector.! index
        typeGroup = typeGroups Vector.! index
    Functor.Term.ModuleSet termGroups =
      tarjan
        Index {(!) = (Functor.Term.!)}
        (map Term.global . freeTerm . indexTerm)
        termIndexes
    Functor.Type.ModuleSet typeGroups =
      tarjan
        Index {(!) = (Functor.Type.!)}
        (map Type.global . freeType . indexType)
        typeIndexes

    freeTerm = foldMap Declaration.groupFree
    freeType = foldMap TypeDeclaration.groupFree

    termIndexes = Functor.Term.ModuleSet $ Vector.generate (length modules) termIndex
    termIndex index =
      Functor.Term.indexes
        (Term.Global index)
        (declarations $ modules Vector.! index)
    typeIndexes = Functor.Type.ModuleSet $ Vector.generate (length modules) typeIndex
    typeIndex index =
      Functor.Type.indexes
        (Type.Global index)
        (declarations $ modules Vector.! index)
    indexTerm' = fromJust . indexTerm
    indexType' = fromJust . indexType
    indexTerm :: Term.Link Locality.Global -> Maybe (Declaration.Groupable Scope.Global)
    indexTerm
      (Term.Global global local)
        | Module {declarations = Declarations {terms}} <- modules Vector.! global =
            Declaration.groupable (terms Vector.! local)
    indexType :: Type.Link Locality.Global -> Maybe (TypeDeclaration.Groupable Scope.Global)
    indexType (Type.Global global local)
      | Module {declarations = Declarations {types}} <- modules Vector.! global =
          TypeDeclaration.groupable (types Vector.! local)

seperate :: Vector (Module Group Check) -> Vector (Module Normal Check)
seperate modules = go <$> modules
  where
    go Module {name, declarations} =
      Module
        { name,
          declarations =
            Declarations.ungroup
              Term.unglobal
              Type.unglobal
              lookupTerm
              lookupType
              declarations
        }
    lookupTerm :: Term.Link Locality.Global -> Implicit (Definition4.Set Locality.Global Check) Check Global
    lookupTerm = \case
      Term.Global global local
        | Module {declarations = Declarations {terms}} <- modules Vector.! global,
          Declaration {definition} <- terms Vector.! local,
          Definition4.Group set <- definition ->
            set
      _ -> error "bad term lookup"
    lookupType :: Type.Link Locality.Global -> TypeDefinition2.Set Locality.Global Check Global
    lookupType = \case
      Type.Global global local
        | Module {declarations = Declarations {types}} <- modules Vector.! global,
          TypeDeclaration {definition} <- types Vector.! local,
          TypeDefinition2.Group set <- definition ->
            set
      _ -> error "bad type lookup"
