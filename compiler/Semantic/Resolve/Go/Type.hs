{-# LANGUAGE_HAZY UnorderedRecords #-}

module Semantic.Resolve.Go.Type where

import qualified Semantic.Index.Constructor as Constructor
import qualified Semantic.Index.Type2 as Type2
import qualified Semantic.Index.Type3 as Type3
import Semantic.Resolve.Context ((!$), (!=*~))
import qualified Semantic.Resolve.Context as Resolved (Context, (!=.*))
import {-# SOURCE #-} qualified Semantic.Resolve.Temporary.TypeInfix as Infix (fix, resolve)
import Semantic.Stage (Equal (..), Resolve)
import Semantic.Tree.Type (Synonym (..), Type (..))
import Syntax.Position (Position)
import qualified Syntax.Tree.Type as Syntax (Type (..))
import qualified Syntax.Tree.TypeInfix as Syntax.TypeInfix
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
            items = resolve context <$> items
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
