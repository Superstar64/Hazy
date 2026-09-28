{-# LANGUAGE_HAZY UnorderedRecords #-}

module Semantic.Tree.Type where

import {-# SOURCE #-} qualified Core.Tree.Type as Simple
import qualified Data.Strict.Vector1 as Strict (Vector1)
import qualified Data.Strict.Vector2 as Strict (Vector2)
import Semantic.FreeVariables (FreeTypeVariables (..))
import qualified Semantic.FreeVariables as FreeVariables
import qualified Semantic.Index.Local as Local
import qualified Semantic.Index.Type2 as Type2
import Semantic.Scope (Environment (..))
import qualified Semantic.Scope as Scope
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Semantic.Stage (Check, Equal (..), IsResolve (..), Unsupported)
import Prelude hiding (Bool (False, True))
import qualified Prelude

data Type position stage scope
  = Variable
      { startPosition :: !position,
        variable :: !(Local.Index scope)
      }
  | Constructor
      { startPosition :: !position,
        constructorPosition :: !position,
        constructor :: !(Type2.Index scope),
        synonym :: !(Synonym stage scope)
      }
  | List
      { startPosition :: !position,
        element :: !(Type position stage scope)
      }
  | Tuple
      { startPosition :: !position,
        elements :: !(Strict.Vector2 (Type position stage scope))
      }
  | Call
      { startPosition :: !position,
        function :: !(Type position stage scope),
        argument :: !(Type position stage scope)
      }
  | Function
      { startPosition :: !position,
        parameter :: !(Type position stage scope),
        operatorPosition :: !position,
        result :: !(Type position stage scope)
      }
  | StrictFunction
      { startPosition :: !position,
        parameter :: !(Type position stage scope),
        operatorPosition :: !position,
        result :: !(Type position stage scope),
        unsupported :: !(Unsupported stage)
      }
  | LiftedList
      { startPosition :: !position,
        items :: !(Strict.Vector1 (Type position stage scope))
      }
  | Type
      { startPosition :: !position,
        universe :: !(Type position stage scope),
        unsupported :: !(Unsupported stage)
      }
  | SmallType
      { startPosition :: !position
      }
  | Constraint
      {startPosition :: !position}
  | Small
      { startPosition :: !position,
        unsupported :: !(Unsupported stage)
      }
  | Large
      { startPosition :: !position,
        unsupported :: !(Unsupported stage)
      }
  | Universe
      { startPosition :: !position,
        unsupported :: !(Unsupported stage)
      }
  | Levity
      {startPosition :: !position}
  deriving (Show, Eq)

instance Shift0.Functor (Type stage position) where
  map = Shift.mapDefault

instance Shift.Functor (Type stage position) where
  map category = \case
    Variable {startPosition, variable} ->
      Variable
        { startPosition,
          variable = Shift.map category variable
        }
    Constructor {startPosition, constructorPosition, constructor, synonym} ->
      Constructor
        { startPosition,
          constructorPosition,
          constructor = Shift.map category constructor,
          synonym = Shift.map category synonym
        }
    List {startPosition, element} ->
      List
        { startPosition,
          element = Shift.map category element
        }
    Tuple {startPosition, elements} ->
      Tuple
        { startPosition,
          elements = fmap (Shift.map category) elements
        }
    Call {startPosition, function, argument} ->
      Call
        { startPosition,
          function = Shift.map category function,
          argument = Shift.map category argument
        }
    Function {startPosition, parameter, operatorPosition, result} ->
      Function
        { startPosition,
          parameter = Shift.map category parameter,
          operatorPosition,
          result = Shift.map category result
        }
    StrictFunction {startPosition, parameter, operatorPosition, result, unsupported} ->
      StrictFunction
        { startPosition,
          parameter = Shift.map category parameter,
          operatorPosition,
          result = Shift.map category result,
          unsupported
        }
    LiftedList {startPosition, items} ->
      LiftedList
        { startPosition,
          items = fmap (Shift.map category) items
        }
    Type {startPosition, universe, unsupported} ->
      Type
        { startPosition,
          universe = Shift.map category universe,
          unsupported
        }
    SmallType {startPosition} -> SmallType {startPosition}
    Constraint {startPosition} -> Constraint {startPosition}
    Small {startPosition, unsupported} -> Small {startPosition, unsupported}
    Large {startPosition, unsupported} -> Large {startPosition, unsupported}
    Universe {startPosition, unsupported} -> Universe {startPosition, unsupported}
    Levity {startPosition} -> Levity {startPosition}

instance FreeTypeVariables (Type position) where
  freeTypeVariables target = \case
    Variable {} -> []
    Constructor {constructor} -> FreeVariables.type2 target constructor
    List {element} -> freeTypeVariables target element
    Tuple {elements} -> foldMap (freeTypeVariables target) elements
    Call {function, argument} ->
      concat
        [ freeTypeVariables target function,
          freeTypeVariables target argument
        ]
    Function {parameter, result} ->
      concat
        [ freeTypeVariables target parameter,
          freeTypeVariables target result
        ]
    StrictFunction {parameter, result} ->
      concat
        [ freeTypeVariables target parameter,
          freeTypeVariables target result
        ]
    LiftedList {items} -> foldMap (freeTypeVariables target) items
    Type {universe} -> freeTypeVariables target universe
    SmallType {} -> []
    Constraint {} -> []
    Small {} -> []
    Large {} -> []
    Universe {} -> []
    Levity {} -> []

data Synonym stage scope where
  NoSynonym :: Synonym stage scope
  Synonym :: !Int -> !(Simple.Type (Scope.Local ':+ scope)) -> Synonym Check scope

instance Show (Synonym stage scope) where
  showsPrec d = \case
    NoSynonym -> showString "NoSynonym"
    Synonym length synonym ->
      showParen (d > 10) $
        showsPrec 11 "Synonym "
          . showsPrec 11 length
          . showString " "
          . showsPrec 11 synonym

instance (IsResolve stage) => Eq (Synonym stage scope) where
  synonym == synonym'
    | Refl <- isResolve :: Unsupported stage,
      NoSynonym <- synonym,
      NoSynonym <- synonym' =
        Prelude.True

instance Shift0.Functor (Synonym stage) where
  map = Shift.mapDefault

instance Shift.Functor (Synonym stage) where
  map category = \case
    NoSynonym -> NoSynonym
    Synonym length synonym -> Synonym length (Shift.map (Shift.Over category) synonym)

anonymize :: Type position stage scope -> Type () stage scope
anonymize = \case
  Variable {variable} ->
    Variable
      { startPosition = (),
        variable
      }
  Constructor {constructor, synonym} ->
    Constructor
      { startPosition = (),
        constructorPosition = (),
        constructor,
        synonym
      }
  List {element} ->
    List
      { startPosition = (),
        element = anonymize element
      }
  Tuple {elements} ->
    Tuple
      { startPosition = (),
        elements = fmap anonymize elements
      }
  Call {function, argument} ->
    Call
      { startPosition = (),
        function = anonymize function,
        argument = anonymize argument
      }
  Function {parameter, result} ->
    Function
      { startPosition = (),
        parameter = anonymize parameter,
        operatorPosition = (),
        result = anonymize result
      }
  StrictFunction {parameter, result, unsupported} ->
    StrictFunction
      { startPosition = (),
        parameter = anonymize parameter,
        operatorPosition = (),
        result = anonymize result,
        unsupported
      }
  LiftedList {items} ->
    LiftedList
      { startPosition = (),
        items = fmap anonymize items
      }
  Type {universe, unsupported} ->
    Type
      { startPosition = (),
        universe = anonymize universe,
        unsupported
      }
  SmallType {} -> SmallType {startPosition = ()}
  Constraint {} -> Constraint {startPosition = ()}
  Small {unsupported} -> Small {startPosition = (), unsupported}
  Large {unsupported} -> Large {startPosition = (), unsupported}
  Universe {unsupported} -> Universe {startPosition = (), unsupported}
  Levity {} -> Levity {startPosition = ()}
