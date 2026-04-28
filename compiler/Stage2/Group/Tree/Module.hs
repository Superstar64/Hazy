module Stage2.Group.Tree.Module where

import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Graph.StronglyConnected (Index (..), tarjan)
import Stage1.Variable (FullQualifiers)
import qualified Stage2.Group.Functor.Term.Declarations as Functor.Term (indexes)
import qualified Stage2.Group.Functor.Term.ModuleSet as Functor.Term (ModuleSet (..), (!))
import qualified Stage2.Group.Functor.Type.Declarations as Functor.Type (indexes)
import qualified Stage2.Group.Functor.Type.ModuleSet as Functor.Type (ModuleSet (..), (!))
import qualified Stage2.Group.Index.Link.Term as Term
import Stage2.Group.Temporary.Declaration (freeGroupTermVariables)
import qualified Stage2.Group.Temporary.Declaration as Temporary
import Stage2.Group.Tree.Declarations (Declarations)
import qualified Stage2.Group.Tree.Declarations as Declarations
import qualified Stage2.Index.Link.Term as Proper.Term
import qualified Stage2.Index.Link.Type as Proper.Type
import qualified Stage2.Index.Link.Type as Type
import Stage2.Locality (Global)
import qualified Stage2.Locality as Locality
import qualified Stage2.Scope as Scope
import qualified Stage2.Tree.Declarations as Proper (Declarations (..))
import qualified Stage2.Tree.Module as Proper (Module (..))
import Stage2.Tree.TypeDeclaration (freeGroupTypeVariables)
import qualified Stage2.Tree.TypeDeclaration as Proper (TypeDeclaration)

data Module = Module
  { name :: !FullQualifiers,
    declarations :: Declarations Global Scope.Global
  }
  deriving (Show)

connect :: Vector Proper.Module -> Vector Module
connect modules = Vector.imap go modules
  where
    go index Proper.Module {name, declarations} =
      Module
        { name,
          declarations = Declarations.group indexTerm indexType termGroup typeGroup declarations
        }
      where
        termGroup = termGroups Vector.! index
        typeGroup = typeGroups Vector.! index
    Functor.Term.ModuleSet termGroups =
      tarjan Index {(!) = (Functor.Term.!)} (fmap Term.global . freeGroupTermVariables . indexTerm) termIndexes
    Functor.Type.ModuleSet typeGroups =
      tarjan Index {(!) = (Functor.Type.!)} (fmap Type.global . freeGroupTypeVariables . indexType) typeIndexes
    termIndexes = Functor.Term.ModuleSet $ Vector.generate (length modules) termIndex
    termIndex index =
      Functor.Term.indexes
        (Proper.Term.Global index)
        (Proper.declarations $ modules Vector.! index)
    typeIndexes = Functor.Type.ModuleSet $ Vector.generate (length modules) typeIndex
    typeIndex index =
      Functor.Type.indexes
        (Proper.Type.Global index)
        (Proper.declarations $ modules Vector.! index)
    indexTerm :: Term.Link Global -> Temporary.Declaration Locality.Global Scope.Global
    indexTerm = \case
      Term.Link (Proper.Term.Global global local)
        | Proper.Module {declarations = Proper.Declarations {terms}} <-
            modules Vector.! global ->
            Temporary.Declaration $ terms Vector.! local
      Term.Share (Proper.Term.Global global local)
        | Proper.Module {declarations = Proper.Declarations {shared}} <-
            modules Vector.! global ->
            Temporary.Shared $ shared Vector.! local
    indexType :: Type.Link Global -> Proper.TypeDeclaration Locality.Global Scope.Global
    indexType = \case
      Type.Global global local
        | Proper.Module {declarations = Proper.Declarations {types}} <-
            modules Vector.! global ->
            types Vector.! local
