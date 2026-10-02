module Semantic.Label.Go.Type where

import Data.Text (pack)
import qualified Data.Vector.Strict as Strict.Vector
import qualified Semantic.Index.Constructor as Constructor
import qualified Semantic.Index.Type2 as Type2
import qualified Semantic.Label.Binding.Type as Label (TypeBinding (..))
import qualified Semantic.Label.Context as Label (Context, (!-.*), (!=.), (!=.*))
import Semantic.Stage (Resolve)
import Semantic.Tree.Type (Type (..))
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
              nonEmpty,
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
        nonEmpty Constructor.Cons1 = builtin ":|"
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
    Type2.NonEmpty -> builtin "NonEmpty"
    Type2.Bool -> builtin "Bool"
    Type2.Char -> builtin "Char"
    Type2.ST -> builtin "ST"
    Type2.Integer -> builtin "Integer"
    Type2.Int -> builtin "Int"
    Type2.Ratio -> builtin "Ratio"
    Type2.Num -> builtin "Num"
    Type2.Enum -> builtin "Enum"
    Type2.Bounded -> builtin "Bounded"
    Type2.Eq -> builtin "Eq"
    Type2.Ord -> builtin "Ord"
    Type2.Real -> builtin "Real"
    Type2.Integral -> builtin "Integral"
    Type2.Fractional -> builtin "Fractional"
    Type2.Functor -> builtin "Functor"
    Type2.Applicative -> builtin "Applicative"
    Type2.Monad -> builtin "Monad"
    Type2.MonadFail -> builtin "MonadFail"
    Type2.Semigroup -> builtin "Semigroup"
    Type2.Monoid -> builtin "Monoid"
    Type2.Show -> builtin "Show"
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
        items = label context <$> items
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
