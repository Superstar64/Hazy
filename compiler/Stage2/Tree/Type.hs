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
import Stage2.FreeVariables (FreeTypeVariables (..))
import qualified Stage2.Index.Constructor as Constructor
import qualified Stage2.Index.Local as Local
import qualified Stage2.Index.Type2 as Type2
import qualified Stage2.Index.Type3 as Type3
import qualified Stage2.Label.Binding.Type as Label (TypeBinding (..))
import qualified Stage2.Label.Context as Label (Context, (!-.*), (!=.), (!=.*))
import Stage2.Resolve.Context ((!$), (!=*~))
import qualified Stage2.Resolve.Context as Resolved (Context, (!=.*))
import Stage2.Scope (Environment (..))
import qualified Stage2.Scope as Scope
import Stage2.Shift (Shift (..), shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Stage (Check, Exclusive (..), Resolve, Unsupported (..))
import {-# SOURCE #-} qualified Stage2.Temporary.TypeInfix as Infix (fix, resolve)
import {-# SOURCE #-} qualified Stage4.Tree.Type as Simple
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

instance FreeTypeVariables (Type stage position) where
  freeTypeVariables target = \case
    Variable {} -> []
    Constructor {constructor} -> freeTypeVariables target constructor
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

instance (Exclusive stage) => Eq (Synonym stage scope) where
  synonym == synonym'
    | Placeholder <- exclusive :: Unsupported stage,
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

resolve :: Resolved.Context scope -> Stage1.Type Position -> Type Position Resolve scope
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
            constructor,
            synonym = NoSynonym
          }
      Type3.Type -> SmallType {startPosition}
      Type3.Constraint -> Constraint {startPosition}
      Type3.Small -> Small {startPosition, unsupported = Placeholder}
      Type3.Large -> Large {startPosition, unsupported = Placeholder}
      Type3.Universe -> Universe {startPosition, unsupported = Placeholder}
      Type3.Levity -> Levity {startPosition}
  Stage1.Unit {startPosition = constructorPosition@startPosition} ->
    Constructor
      { startPosition,
        constructorPosition,
        constructor = Type2.Tuple 0,
        synonym = NoSynonym
      }
  Stage1.Arrow {startPosition = constructorPosition@startPosition} ->
    Constructor
      { startPosition,
        constructorPosition,
        constructor = Type2.Arrow,
        synonym = NoSynonym
      }
  Stage1.Listing {startPosition = constructorPosition@startPosition} ->
    Constructor
      { startPosition,
        constructorPosition,
        constructor = Type2.List,
        synonym = NoSynonym
      }
  Stage1.Tupling {startPosition = constructorPosition@startPosition, count} ->
    Constructor
      { startPosition,
        constructorPosition,
        constructor = Type2.Tuple count,
        synonym = NoSynonym
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
  Stage1.Call {startPosition, function, argument} ->
    Call
      { startPosition,
        function = resolve context function,
        argument = resolve context argument
      }
  Stage1.Function {startPosition, parameter, operatorPosition, result} ->
    Function
      { startPosition,
        parameter = resolve context parameter,
        operatorPosition,
        result = resolve context result
      }
  Stage1.StrictFunction {startPosition, parameter, operatorPosition, result} ->
    StrictFunction
      { startPosition,
        parameter = resolve context parameter,
        operatorPosition,
        result = resolve context result,
        unsupported = Placeholder
      }
  Stage1.Lifted {startPosition = constructorPosition@startPosition, lifted} ->
    Constructor
      { startPosition,
        constructorPosition,
        constructor = Type2.Lifted (context !=*~ lifted),
        synonym = NoSynonym
      }
  Stage1.LiftedCons {startPosition = constructorPosition@startPosition} ->
    Constructor
      { startPosition,
        constructorPosition,
        constructor = Type2.Lifted Constructor.cons,
        synonym = NoSynonym
      }
  Stage1.LiftedList {startPosition = constructorPosition@startPosition, items}
    | null items ->
        Constructor
          { startPosition,
            constructorPosition,
            constructor = Type2.Lifted Constructor.nil,
            synonym = NoSynonym
          }
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
        universe = resolve context universe,
        unsupported = Placeholder
      }
  Stage1.Star {startPosition} -> SmallType {startPosition}

label :: Label.Context scope -> Type unit Resolve scope -> Stage1.Type ()
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
              tuplex,
              ordering,
              ratio
            }
        bool Constructor.False = builtin "False"
        bool Constructor.True = builtin "True"
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
        ordering Constructor.LT = builtin "LT"
        ordering Constructor.EQ = builtin "EQ"
        ordering Constructor.GT = builtin "GT"
        ratio Constructor.MakeRatio = builtin ":%"
        builtin name =
          Stage1.Lifted
            { startPosition = (),
              lifted = () :@ hazy := ConstructorIdentifier (constructorIdentifier $ pack name)
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
        Stage1.Constructor
          { startPosition = (),
            constructor = () :@ hazy :=. constructorIdentifier (pack name)
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
  Levity {} ->
    Stage1.Constructor
      { startPosition = (),
        constructor = () :@ hazy :=. constructorIdentifier (pack "Levity")
      }
  where
    hazy = Local :. constructorIdentifier (pack "Hazy")
