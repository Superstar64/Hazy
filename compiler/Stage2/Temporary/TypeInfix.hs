module Stage2.Temporary.TypeInfix where

import Stage1.Position (Position)
import Stage1.Tree.Associativity (Associativity (..))
import Stage1.Tree.Fixity (Fixity (..))
import Stage1.Tree.Marked (Marked ((:@)))
import qualified Stage1.Tree.TypeInfix as Stage1 (Infix (..))
import qualified Stage2.Index.Constructor as Constructor (cons)
import qualified Stage2.Index.Type2 as Type2
import qualified Stage2.Resolve.Binding.Constructor as Constructor (Binding (..))
import Stage2.Resolve.Context
  ( Context (..),
    (!=~),
  )
import Stage2.Temporary.Infix (Infix (..))
import qualified Stage2.Temporary.Infix as Infix
import Stage2.Tree.Type (Type)
import qualified Stage2.Tree.Type as Type
  ( Type (Call, Constructor, argument, constructor, constructorPosition, function, startPosition),
    resolve,
  )
import Prelude hiding (Either (Left, Right))

data Index scope
  = Constructor !Position !(Constructor.Binding scope)
  | Cons !Position

fixWith :: Maybe Associativity -> Int -> Infix (Index scope) (Type Position scope) -> Type Position scope
fixWith = Infix.fixWith position fixity operator
  where
    position = \case
      Constructor position _ -> position
      Cons position -> position
    fixity :: Index scope -> Fixity
    fixity = \case
      Constructor _ Constructor.Binding {fixity} -> fixity
      Cons _ -> Fixity Right 5

    operator :: Type Position scope -> Index scope -> Type Position scope -> Type Position scope
    operator left operator right = case operator of
      Constructor constructorPosition Constructor.Binding {index} ->
        Type.Constructor
          { startPosition = Type.startPosition left,
            constructorPosition,
            constructor = Type2.Lifted index
          }
          `call` left
          `call` right
      Cons constructorPosition ->
        Type.Constructor
          { startPosition = Type.startPosition left,
            constructorPosition,
            constructor = Type2.Lifted Constructor.cons
          }
          `call` left
          `call` right
      where
        call function argument =
          Type.Call
            { function,
              argument
            }

fix :: Infix (Index scope) (Type Position scope) -> Type Position scope
fix = fixWith Nothing 0

resolve :: Context scope -> Stage1.Infix Position -> Infix (Index scope) (Type Position scope)
resolve context = \case
  Stage1.Type type1 -> Single (Type.resolve context type1)
  Stage1.Infix {left, operator = operator@(operatorPosition :@ _), right} ->
    Infix
      (Type.resolve context left)
      (Constructor operatorPosition $ context !=~ operator)
      (resolve context right)
  Stage1.InfixCons {head, operatorPosition, tail} ->
    Infix (Type.resolve context head) (Cons operatorPosition) (resolve context tail)
