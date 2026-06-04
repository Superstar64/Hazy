{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage2.Resolve.Go.Type where

import qualified Data.Strict.Vector1 as Strict.Vector1
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
import qualified Stage2.Index.Type2 as Type2
import qualified Stage2.Index.Type3 as Type3
import qualified Stage2.Label.Binding.Type as Label (TypeBinding (..))
import qualified Stage2.Label.Context as Label (Context, (!-.*), (!=.), (!=.*))
import Stage2.Resolve.Context ((!$), (!=*~))
import qualified Stage2.Resolve.Context as Resolved (Context, (!=.*))
import Stage2.Stage (Equal (..), Resolve)
import {-# SOURCE #-} qualified Stage2.Temporary.TypeInfix as Infix (fix, resolve)
import Stage2.Tree.Type (Synonym (..), Type (..))
import Prelude hiding (Bool (False, True))

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
      Type3.Small -> Small {startPosition, unsupported = Refl}
      Type3.Large -> Large {startPosition, unsupported = Refl}
      Type3.Universe -> Universe {startPosition, unsupported = Refl}
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
        unsupported = Refl
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
        unsupported = Refl
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
