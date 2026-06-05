{-# LANGUAGE_HAZY UnorderedRecords #-}

module Semantic.Resolve.Go.Type where

import qualified Data.Strict.Vector1 as Strict.Vector1
import Data.Text (pack)
import qualified Data.Vector.Strict as Strict.Vector
import qualified Semantic.Index.Constructor as Constructor
import qualified Semantic.Index.Type2 as Type2
import qualified Semantic.Index.Type3 as Type3
import qualified Semantic.Label.Binding.Type as Label (TypeBinding (..))
import qualified Semantic.Label.Context as Label (Context, (!-.*), (!=.), (!=.*))
import Semantic.Resolve.Context ((!$), (!=*~))
import qualified Semantic.Resolve.Context as Resolved (Context, (!=.*))
import {-# SOURCE #-} qualified Semantic.Resolve.Temporary.TypeInfix as Infix (fix, resolve)
import Semantic.Stage (Equal (..), Resolve)
import Semantic.Tree.Type (Synonym (..), Type (..))
import Syntax.Lexer (constructorIdentifier)
import Syntax.Position (Position)
import Syntax.Tree.Marked (Marked (..))
import qualified Syntax.Tree.Type as Syntax (Type (..))
import qualified Syntax.Tree.TypeInfix as Syntax.TypeInfix
import Syntax.Variable
  ( Constructor (ConstructorIdentifier),
    QualifiedConstructor ((:=)),
    QualifiedConstructorIdentifier (..),
    Qualifiers (..),
  )
import Prelude hiding (Bool (False, True))

resolve :: Resolved.Context scope -> Syntax.Type Position -> Type Position Resolve scope
resolve context = \case
  Syntax.Variable {startPosition, variable} ->
    Variable
      { startPosition,
        variable = context !$ variable
      }
  Syntax.Constructor {startPosition = constructorPosition@startPosition, constructor} ->
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
  Syntax.Unit {startPosition = constructorPosition@startPosition} ->
    Constructor
      { startPosition,
        constructorPosition,
        constructor = Type2.Tuple 0,
        synonym = NoSynonym
      }
  Syntax.Arrow {startPosition = constructorPosition@startPosition} ->
    Constructor
      { startPosition,
        constructorPosition,
        constructor = Type2.Arrow,
        synonym = NoSynonym
      }
  Syntax.Listing {startPosition = constructorPosition@startPosition} ->
    Constructor
      { startPosition,
        constructorPosition,
        constructor = Type2.List,
        synonym = NoSynonym
      }
  Syntax.Tupling {startPosition = constructorPosition@startPosition, count} ->
    Constructor
      { startPosition,
        constructorPosition,
        constructor = Type2.Tuple count,
        synonym = NoSynonym
      }
  Syntax.List {startPosition, element} ->
    List
      { startPosition,
        element = resolve context element
      }
  Syntax.Tuple {startPosition, elements} ->
    Tuple
      { startPosition,
        elements = fmap (resolve context) elements
      }
  Syntax.Call {startPosition, function, argument} ->
    Call
      { startPosition,
        function = resolve context function,
        argument = resolve context argument
      }
  Syntax.Function {startPosition, parameter, operatorPosition, result} ->
    Function
      { startPosition,
        parameter = resolve context parameter,
        operatorPosition,
        result = resolve context result
      }
  Syntax.StrictFunction {startPosition, parameter, operatorPosition, result} ->
    StrictFunction
      { startPosition,
        parameter = resolve context parameter,
        operatorPosition,
        result = resolve context result,
        unsupported = Refl
      }
  Syntax.Lifted {startPosition = constructorPosition@startPosition, lifted} ->
    Constructor
      { startPosition,
        constructorPosition,
        constructor = Type2.Lifted (context !=*~ lifted),
        synonym = NoSynonym
      }
  Syntax.LiftedCons {startPosition = constructorPosition@startPosition} ->
    Constructor
      { startPosition,
        constructorPosition,
        constructor = Type2.Lifted Constructor.cons,
        synonym = NoSynonym
      }
  Syntax.LiftedList {startPosition = constructorPosition@startPosition, items}
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
  Syntax.Infix {startPosition, left, operator, right} ->
    Infix.fix operators
    where
      operators =
        Infix.resolve context $
          Syntax.TypeInfix.Infix
            { startPosition,
              left,
              operator,
              right
            }
  Syntax.InfixCons {startPosition, head, operatorPosition, tail} ->
    Infix.fix operators
    where
      operators =
        Infix.resolve context $
          Syntax.TypeInfix.InfixCons
            { startPosition,
              head,
              operatorPosition,
              tail
            }
  Syntax.Type {startPosition, universe} ->
    Type
      { startPosition,
        universe = resolve context universe,
        unsupported = Refl
      }
  Syntax.Star {startPosition} -> SmallType {startPosition}

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
