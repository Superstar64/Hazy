module Semantic.Resolve.Temporary.TypeInfix where

import qualified Semantic.Index.Constructor as Constructor (cons)
import qualified Semantic.Index.Type2 as Type2
import qualified Semantic.Resolve.Binding.Constructor as Constructor (Binding (..))
import Semantic.Resolve.Context
  ( Context (..),
    (!=~),
  )
import qualified Semantic.Resolve.Go.Type as Type (resolve)
import Semantic.Resolve.Temporary.Infix (Infix (..))
import qualified Semantic.Resolve.Temporary.Infix as Infix
import Semantic.Stage (Resolve)
import Semantic.Tree.Type (Synonym (..), Type)
import qualified Semantic.Tree.Type as Type
  ( Type
      ( Call,
        Constructor,
        argument,
        constructor,
        constructorPosition,
        function,
        startPosition,
        synonym
      ),
  )
import Syntax.Position (Position)
import Syntax.Tree.Associativity (Associativity (..))
import Syntax.Tree.Fixity (Fixity (..))
import Syntax.Tree.Marked (Marked ((:@)))
import qualified Syntax.Tree.TypeInfix as Syntax (Infix (..))
import Prelude hiding (Either (Left, Right))

data Index scope
  = Constructor !Position !(Constructor.Binding scope)
  | Cons !Position

fixWith ::
  Maybe Associativity ->
  Int ->
  Infix (Index scope) (Type Position Resolve scope) ->
  Type Position Resolve scope
fixWith = Infix.fixWith position fixity operator
  where
    position = \case
      Constructor position _ -> position
      Cons position -> position
    fixity :: Index scope -> Fixity
    fixity = \case
      Constructor _ Constructor.Binding {fixity} -> fixity
      Cons _ -> Fixity {associativity = Right, precedence = 5}

    operator ::
      Type Position Resolve scope ->
      Index scope ->
      Type Position Resolve scope ->
      Type Position Resolve scope
    operator left operator right = case operator of
      Constructor constructorPosition Constructor.Binding {index} ->
        Type.Constructor
          { startPosition = Type.startPosition left,
            constructorPosition,
            constructor = Type2.Lifted index,
            synonym = NoSynonym
          }
          `call` left
          `call` right
      Cons constructorPosition ->
        Type.Constructor
          { startPosition = Type.startPosition left,
            constructorPosition,
            constructor = Type2.Lifted Constructor.cons,
            synonym = NoSynonym
          }
          `call` left
          `call` right
      where
        call function argument =
          Type.Call
            { startPosition = Type.startPosition function,
              function,
              argument
            }

fix :: Infix (Index scope) (Type Position Resolve scope) -> Type Position Resolve scope
fix = fixWith Nothing 0

resolve :: Context scope -> Syntax.Infix Position -> Infix (Index scope) (Type Position Resolve scope)
resolve context = \case
  Syntax.Type type1 -> Single (Type.resolve context type1)
  Syntax.Infix {left, operator = operator@(operatorPosition :@ _), right} ->
    Infix
      (Type.resolve context left)
      (Constructor operatorPosition $ context !=~ operator)
      (resolve context right)
  Syntax.InfixCons {head, operatorPosition, tail} ->
    Infix (Type.resolve context head) (Cons operatorPosition) (resolve context tail)
