{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage2.Tree.Type where

import qualified Data.Strict.Vector1 as Strict (Vector1)
import qualified Data.Strict.Vector1 as Strict.Vector1
import qualified Data.Strict.Vector2 as Strict (Vector2)
import Data.Text (pack)
import qualified Data.Vector.Strict as Strict.Vector
import Stage1.Lexer (constructorIdentifier)
import Stage1.Position (Position)
import Stage1.Tree.Marked (Marked (..))
import qualified Stage1.Tree.Type as Stage1 (Type (..))
import qualified Stage1.Tree.TypeInfix as Stage1.TypeInfix
import Stage1.Variable
  ( Constructor (ConstructorIdentifier),
    QualifiedConstructor ((:=)),
    QualifiedConstructorIdentifier (..),
    Qualifiers (..),
  )
import qualified Stage2.Index.Constructor as Constructor
import qualified Stage2.Index.Local as Local
import qualified Stage2.Index.Type2 as Type2
import qualified Stage2.Index.Type3 as Type3
import qualified Stage2.Label.Binding.Type as Label (TypeBinding (..))
import qualified Stage2.Label.Context as Label (Context, (!-.*), (!=.), (!=.*))
import Stage2.Resolve.Context ((!$), (!=*~))
import qualified Stage2.Resolve.Context as Resolved (Context, (!=.*))
import Stage2.Shift (Shift (..), shiftDefault)
import qualified Stage2.Shift as Shift
import {-# SOURCE #-} qualified Stage2.Temporary.TypeInfix as Infix (fix, resolve)
import Prelude hiding (Bool (False, True))

data Type position scope
  = Variable
      { startPosition :: !position,
        variable :: !(Local.Index scope)
      }
  | Constructor
      { startPosition :: !position,
        constructorPosition :: !position,
        constructor :: !(Type2.Index scope)
      }
  | List
      { startPosition :: !position,
        element :: !(Type position scope)
      }
  | Tuple
      { startPosition :: !position,
        elements :: !(Strict.Vector2 (Type position scope))
      }
  | Call
      { function :: !(Type position scope),
        argument :: !(Type position scope)
      }
  | Function
      { parameter :: !(Type position scope),
        operatorPosition :: !position,
        result :: !(Type position scope)
      }
  | StrictFunction
      { parameter :: !(Type position scope),
        operatorPosition :: !position,
        result :: !(Type position scope)
      }
  | LiftedList
      { startPosition :: !position,
        items :: !(Strict.Vector1 (Type position scope))
      }
  | Type
      { startPosition :: !position,
        universe :: !(Type position scope)
      }
  | SmallType
      { startPosition :: !position
      }
  | Constraint
      {startPosition :: !position}
  | Small
      {startPosition :: !position}
  | Large
      {startPosition :: !position}
  | Universe
      {startPosition :: !position}
  deriving (Show, Eq)

instance Shift (Type position) where
  shift = shiftDefault

instance Shift.Functor (Type position) where
  map category = \case
    Variable {startPosition, variable} ->
      Variable
        { startPosition,
          variable = Shift.map category variable
        }
    Constructor {startPosition, constructorPosition, constructor} ->
      Constructor
        { startPosition,
          constructorPosition,
          constructor = Shift.map category constructor
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
    Call {function, argument} ->
      Call
        { function = Shift.map category function,
          argument = Shift.map category argument
        }
    Function {parameter, operatorPosition, result} ->
      Function
        { parameter = Shift.map category parameter,
          operatorPosition,
          result = Shift.map category result
        }
    StrictFunction {parameter, operatorPosition, result} ->
      StrictFunction
        { parameter = Shift.map category parameter,
          operatorPosition,
          result = Shift.map category result
        }
    LiftedList {startPosition, items} ->
      LiftedList
        { startPosition,
          items = fmap (Shift.map category) items
        }
    Type {startPosition, universe} ->
      Type
        { startPosition,
          universe = Shift.map category universe
        }
    SmallType {startPosition} -> SmallType {startPosition}
    Constraint {startPosition} -> Constraint {startPosition}
    Small {startPosition} -> Small {startPosition}
    Large {startPosition} -> Large {startPosition}
    Universe {startPosition} -> Universe {startPosition}

anonymize :: Type position scope -> Type () scope
anonymize = \case
  Variable {variable} ->
    Variable
      { startPosition = (),
        variable
      }
  Constructor {constructor} ->
    Constructor
      { startPosition = (),
        constructorPosition = (),
        constructor
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
      { function = anonymize function,
        argument = anonymize argument
      }
  Function {parameter, result} ->
    Function
      { parameter = anonymize parameter,
        operatorPosition = (),
        result = anonymize result
      }
  StrictFunction {parameter, result} ->
    StrictFunction
      { parameter = anonymize parameter,
        operatorPosition = (),
        result = anonymize result
      }
  LiftedList {items} ->
    LiftedList
      { startPosition = (),
        items = fmap anonymize items
      }
  Type {universe} ->
    Type
      { startPosition = (),
        universe = anonymize universe
      }
  SmallType {} -> SmallType {startPosition = ()}
  Constraint {} -> Constraint {startPosition = ()}
  Small {} -> Small {startPosition = ()}
  Large {} -> Large {startPosition = ()}
  Universe {} -> Universe {startPosition = ()}

resolve :: Resolved.Context scope -> Stage1.Type Position -> Type Position scope
resolve context = \case
  Stage1.Variable {startPosition, variable} ->
    Variable
      { startPosition,
        variable = context !$ variable
      }
  Stage1.Constructor {startPosition = constructorPosition@startPosition, constructor} ->
    case context Resolved.!=.* constructor of
      Type3.Index constructor ->
        Constructor
          { startPosition,
            constructorPosition,
            constructor
          }
      Type3.Type -> SmallType {startPosition}
      Type3.Constraint -> Constraint {startPosition}
      Type3.Small -> Small {startPosition}
      Type3.Large -> Large {startPosition}
      Type3.Universe -> Universe {startPosition}
  Stage1.Unit {startPosition = constructorPosition@startPosition} ->
    Constructor
      { startPosition,
        constructorPosition,
        constructor = Type2.Tuple 0
      }
  Stage1.Arrow {startPosition = constructorPosition@startPosition} ->
    Constructor
      { startPosition,
        constructorPosition,
        constructor = Type2.Arrow
      }
  Stage1.Listing {startPosition = constructorPosition@startPosition} ->
    Constructor
      { startPosition,
        constructorPosition,
        constructor = Type2.List
      }
  Stage1.Tupling {startPosition = constructorPosition@startPosition, count} ->
    Constructor
      { startPosition,
        constructorPosition,
        constructor = Type2.Tuple count
      }
  Stage1.List {startPosition, element} ->
    List
      { startPosition,
        element = resolve context element
      }
  Stage1.Tuple {startPosition, elements} ->
    Tuple
      { startPosition,
        elements = fmap (resolve context) elements
      }
  Stage1.Call {function, argument} ->
    Call
      { function = resolve context function,
        argument = resolve context argument
      }
  Stage1.Function {parameter, operatorPosition, result} ->
    Function
      { parameter = resolve context parameter,
        operatorPosition,
        result = resolve context result
      }
  Stage1.StrictFunction {parameter, operatorPosition, result} ->
    StrictFunction
      { parameter = resolve context parameter,
        operatorPosition,
        result = resolve context result
      }
  Stage1.Lifted {startPosition = constructorPosition@startPosition, lifted} ->
    Constructor
      { startPosition,
        constructorPosition,
        constructor = Type2.Lifted (context !=*~ lifted)
      }
  Stage1.LiftedCons {startPosition = constructorPosition@startPosition} ->
    Constructor
      { startPosition,
        constructorPosition,
        constructor = Type2.Lifted Constructor.cons
      }
  Stage1.LiftedList {startPosition = constructorPosition@startPosition, items}
    | null items -> Constructor {startPosition, constructorPosition, constructor = Type2.Lifted Constructor.nil}
    | otherwise ->
        LiftedList
          { startPosition,
            items = resolve context <$> Strict.Vector1.fromVector items
          }
  Stage1.Infix {startPosition, left, operator, right} ->
    Infix.fix operators
    where
      operators =
        Infix.resolve context $
          Stage1.TypeInfix.Infix
            { startPosition,
              left,
              operator,
              right
            }
  Stage1.InfixCons {startPosition, head, operatorPosition, tail} ->
    Infix.fix operators
    where
      operators =
        Infix.resolve context $
          Stage1.TypeInfix.InfixCons
            { startPosition,
              head,
              operatorPosition,
              tail
            }
  Stage1.Type {startPosition, universe} ->
    Type
      { startPosition,
        universe = resolve context universe
      }
  Stage1.Star {startPosition} -> SmallType {startPosition}

label :: Label.Context scope -> Type unit scope -> Stage1.Type ()
label context = \case
  Variable {variable} ->
    Stage1.Variable
      { startPosition = (),
        variable = () :@ context Label.!-.* variable
      }
  Constructor {constructor} -> case constructor of
    Type2.Index constructor ->
      Stage1.Constructor
        { startPosition = (),
          constructor = () :@ context Label.!=.* constructor
        }
    Type2.Lifted lifted ->
      Constructor.run normal all lifted
      where
        normal typeIndex constructorIndex
          | Label.TypeBinding {constructorNames} <- context Label.!=. typeIndex =
              Stage1.Lifted
                { startPosition = (),
                  lifted = () :@ constructorNames Strict.Vector.! constructorIndex
                }
        all =
          Constructor.All
            { bool,
              list,
              tuplex
            }
        bool Constructor.False =
          Stage1.Lifted
            { startPosition = (),
              lifted = () :@ hazy := ConstructorIdentifier (constructorIdentifier $ pack "False")
            }
        bool Constructor.True =
          Stage1.Lifted
            { startPosition = (),
              lifted = () :@ hazy := ConstructorIdentifier (constructorIdentifier $ pack "True")
            }
        list Constructor.Nil =
          Stage1.LiftedList
            { startPosition = (),
              items = Strict.Vector.empty
            }
        list Constructor.Cons =
          Stage1.LiftedCons
            { startPosition = ()
            }
        tuplex count _ =
          Stage1.Tupling
            { startPosition = (),
              count
            }
    Type2.Bool ->
      Stage1.Constructor
        { startPosition = (),
          constructor = () :@ hazy :=. constructorIdentifier (pack "Bool")
        }
    Type2.Char ->
      Stage1.Constructor
        { startPosition = (),
          constructor = () :@ hazy :=. constructorIdentifier (pack "Char")
        }
    Type2.ST ->
      Stage1.Constructor
        { startPosition = (),
          constructor = () :@ hazy :=. constructorIdentifier (pack "ST")
        }
    Type2.Arrow ->
      Stage1.Arrow
        { startPosition = ()
        }
    Type2.List ->
      Stage1.Listing
        { startPosition = ()
        }
    Type2.Tuple count ->
      Stage1.Tupling
        { startPosition = (),
          count
        }
    Type2.Integer ->
      Stage1.Constructor
        { startPosition = (),
          constructor = () :@ hazy :=. constructorIdentifier (pack "Integer")
        }
    Type2.Int ->
      Stage1.Constructor
        { startPosition = (),
          constructor = () :@ hazy :=. constructorIdentifier (pack "Int")
        }
    Type2.Num ->
      Stage1.Constructor
        { startPosition = (),
          constructor = () :@ hazy :=. constructorIdentifier (pack "Num")
        }
    Type2.Enum ->
      Stage1.Constructor
        { startPosition = (),
          constructor = () :@ hazy :=. constructorIdentifier (pack "Enum")
        }
  List {element} ->
    Stage1.List
      { startPosition = (),
        element = label context element
      }
  Tuple {elements} ->
    Stage1.Tuple
      { startPosition = (),
        elements = fmap (label context) elements
      }
  Call {function, argument} ->
    Stage1.Call
      { startPosition = (),
        function = label context function,
        argument = label context argument
      }
  Function {parameter, result} ->
    Stage1.Function
      { startPosition = (),
        parameter = label context parameter,
        operatorPosition = (),
        result = label context result
      }
  StrictFunction {parameter, result} ->
    Stage1.StrictFunction
      { startPosition = (),
        parameter = label context parameter,
        operatorPosition = (),
        result = label context result
      }
  LiftedList {items} ->
    Stage1.LiftedList
      { startPosition = (),
        items = Strict.Vector1.toVector $ label context <$> items
      }
  Type {universe} ->
    Stage1.Type
      { startPosition = (),
        universe = label context universe
      }
  Constraint {} ->
    Stage1.Constructor
      { startPosition = (),
        constructor = () :@ hazy :=. constructorIdentifier (pack "Constraint")
      }
  Small {} ->
    Stage1.Constructor
      { startPosition = (),
        constructor = () :@ hazy :=. constructorIdentifier (pack "Small")
      }
  Large {} ->
    Stage1.Constructor
      { startPosition = (),
        constructor = () :@ hazy :=. constructorIdentifier (pack "Large")
      }
  Universe {} ->
    Stage1.Constructor
      { startPosition = (),
        constructor = () :@ hazy :=. constructorIdentifier (pack "Universe")
      }
  SmallType {} ->
    Stage1.Constructor
      { startPosition = (),
        constructor = () :@ hazy :=. constructorIdentifier (pack "Type")
      }
  where
    hazy = Local :. constructorIdentifier (pack "Hazy")
