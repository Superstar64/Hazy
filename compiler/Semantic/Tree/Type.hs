{-# LANGUAGE_HAZY UnorderedRecords #-}

module Semantic.Tree.Type where

import {-# SOURCE #-} qualified Core.Tree.Type as Simple
import qualified Data.Strict.Vector1 as Strict (Vector1)
import qualified Data.Strict.Vector1 as Strict.Vector1
import qualified Data.Strict.Vector2 as Strict (Vector2)
import Data.Text (pack)
import qualified Data.Vector.Strict as Strict.Vector
import Semantic.FreeVariables (FreeTypeVariables (..))
import qualified Semantic.FreeVariables as FreeVariables
import qualified Semantic.Index.Constructor as Constructor
import qualified Semantic.Index.Local as Local
import qualified Semantic.Index.Type2 as Type2
import qualified Semantic.Label.Binding.Type as Label (TypeBinding (..))
import qualified Semantic.Label.Context as Label (Context, (!-.*), (!=.), (!=.*))
import Semantic.Scope (Environment (..))
import qualified Semantic.Scope as Scope
import Semantic.Shift (Shift (..), shiftDefault)
import qualified Semantic.Shift as Shift
import Semantic.Stage (Check, Equal (..), IsResolve (..), Resolve, Unsupported)
import Syntax.Lexer (constructorIdentifier)
import Syntax.Tree.Marked (Marked (..))
import qualified Syntax.Tree.Type as Syntax (Type (..))
import Syntax.Variable
  ( Constructor (ConstructorIdentifier),
    QualifiedConstructor ((:=)),
    QualifiedConstructorIdentifier (..),
    Qualifiers (..),
  )
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

instance Shift (Type stage position) where
  shift = shiftDefault

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
  Synonym :: !(Simple.Type (Scope.Local ':+ scope)) -> Synonym Check scope

instance Show (Synonym stage scope) where
  showsPrec d = \case
    NoSynonym -> showString "NoSynonym"
    Synonym synonym -> showParen (d > 10) $ showsPrec 11 "Synonym " . showsPrec 11 synonym

instance (IsResolve stage) => Eq (Synonym stage scope) where
  synonym == synonym'
    | Refl <- isResolve :: Unsupported stage,
      NoSynonym <- synonym,
      NoSynonym <- synonym' =
        Prelude.True

instance Shift (Synonym stage) where
  shift = shiftDefault

instance Shift.Functor (Synonym stage) where
  map category = \case
    NoSynonym -> NoSynonym
    Synonym synonym -> Synonym (Shift.map (Shift.Over category) synonym)

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

label :: Label.Context scope -> Type unit Resolve scope -> Syntax.Type ()
label context = \case
  Variable {variable} ->
    Syntax.Variable
      { startPosition = (),
        variable = () :@ context Label.!-.* variable
      }
  Constructor {constructor} -> case constructor of
    Type2.Index constructor ->
      Syntax.Constructor
        { startPosition = (),
          constructor = () :@ context Label.!=.* constructor
        }
    Type2.Lifted lifted ->
      Constructor.run normal all lifted
      where
        normal typeIndex constructorIndex
          | Label.TypeBinding {constructorNames} <- context Label.!=. typeIndex =
              Syntax.Lifted
                { startPosition = (),
                  lifted = () :@ constructorNames Strict.Vector.! constructorIndex
                }
        all =
          Constructor.All
            { bool,
              list,
              tuplex,
              ordering,
              ratio
            }
        bool Constructor.False = builtin "False"
        bool Constructor.True = builtin "True"
        list Constructor.Nil =
          Syntax.LiftedList
            { startPosition = (),
              items = Strict.Vector.empty
            }
        list Constructor.Cons =
          Syntax.LiftedCons
            { startPosition = ()
            }
        tuplex count _ =
          Syntax.Tupling
            { startPosition = (),
              count
            }
        ordering Constructor.LT = builtin "LT"
        ordering Constructor.EQ = builtin "EQ"
        ordering Constructor.GT = builtin "GT"
        ratio Constructor.MakeRatio = builtin ":%"
        builtin name =
          Syntax.Lifted
            { startPosition = (),
              lifted = () :@ hazy := ConstructorIdentifier (constructorIdentifier $ pack name)
            }
    Type2.Arrow ->
      Syntax.Arrow
        { startPosition = ()
        }
    Type2.List ->
      Syntax.Listing
        { startPosition = ()
        }
    Type2.Tuple count ->
      Syntax.Tupling
        { startPosition = (),
          count
        }
    Type2.Bool -> builtin "Bool"
    Type2.Char -> builtin "Char"
    Type2.ST -> builtin "ST"
    Type2.Integer -> builtin "Integer"
    Type2.Int -> builtin "Int"
    Type2.Ratio -> builtin "Ratio"
    Type2.Num -> builtin "Num"
    Type2.Enum -> builtin "Enum"
    Type2.Eq -> builtin "Eq"
    Type2.Ord -> builtin "Ord"
    Type2.Real -> builtin "Real"
    Type2.Integral -> builtin "Integral"
    Type2.Fractional -> builtin "Fractional"
    Type2.Functor -> builtin "Functor"
    Type2.Applicative -> builtin "Applicative"
    Type2.Monad -> builtin "Monad"
    Type2.MonadFail -> builtin "MonadFail"
    Type2.Ordering -> builtin "Ordering"
    Type2.Lazy -> builtin "Lazy"
    Type2.Strict -> builtin "Strict"
    where
      builtin name =
        Syntax.Constructor
          { startPosition = (),
            constructor = () :@ hazy :=. constructorIdentifier (pack name)
          }
  List {element} ->
    Syntax.List
      { startPosition = (),
        element = label context element
      }
  Tuple {elements} ->
    Syntax.Tuple
      { startPosition = (),
        elements = fmap (label context) elements
      }
  Call {function, argument} ->
    Syntax.Call
      { startPosition = (),
        function = label context function,
        argument = label context argument
      }
  Function {parameter, result} ->
    Syntax.Function
      { startPosition = (),
        parameter = label context parameter,
        operatorPosition = (),
        result = label context result
      }
  StrictFunction {parameter, result} ->
    Syntax.StrictFunction
      { startPosition = (),
        parameter = label context parameter,
        operatorPosition = (),
        result = label context result
      }
  LiftedList {items} ->
    Syntax.LiftedList
      { startPosition = (),
        items = Strict.Vector1.toVector $ label context <$> items
      }
  Type {universe} ->
    Syntax.Type
      { startPosition = (),
        universe = label context universe
      }
  Constraint {} ->
    Syntax.Constructor
      { startPosition = (),
        constructor = () :@ hazy :=. constructorIdentifier (pack "Constraint")
      }
  Small {} ->
    Syntax.Constructor
      { startPosition = (),
        constructor = () :@ hazy :=. constructorIdentifier (pack "Small")
      }
  Large {} ->
    Syntax.Constructor
      { startPosition = (),
        constructor = () :@ hazy :=. constructorIdentifier (pack "Large")
      }
  Universe {} ->
    Syntax.Constructor
      { startPosition = (),
        constructor = () :@ hazy :=. constructorIdentifier (pack "Universe")
      }
  SmallType {} ->
    Syntax.Constructor
      { startPosition = (),
        constructor = () :@ hazy :=. constructorIdentifier (pack "Type")
      }
  Levity {} ->
    Syntax.Constructor
      { startPosition = (),
        constructor = () :@ hazy :=. constructorIdentifier (pack "Levity")
      }
  where
    hazy = Local :. constructorIdentifier (pack "Hazy")
