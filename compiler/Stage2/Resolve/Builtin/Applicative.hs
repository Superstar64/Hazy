module Stage2.Resolve.Builtin.Applicative (pure, ap, liftA2, discardLeft, discardRight, applicative) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (pack)
import Stage1.Lexer (constructorIdentifier, variableIdentifier, variableSymbol)
import qualified Stage1.Position as Position
import Stage1.Tree.Associativity (Associativity (..))
import Stage1.Tree.Fixity (Fixity (..))
import Stage1.Variable (Variable (..))
import qualified Stage2.Index.Method as Method
import qualified Stage2.Index.Term2 as Term2
import qualified Stage2.Index.Type2 as Type2
import qualified Stage2.Index.Type3 as Type3
import Stage2.Resolve.Binding.Term (Selector (Normal))
import qualified Stage2.Resolve.Binding.Term as Term
import qualified Stage2.Resolve.Binding.Type as Type
import Prelude hiding (Either (..), liftA2, pure)
import qualified Prelude

applicativeName = constructorIdentifier (pack "Applicative")

pureName = VariableIdentifier $ variableIdentifier (pack "pure")

apName = VariableSymbol $ variableSymbol (pack "<*>")

liftA2Name = VariableIdentifier $ variableIdentifier (pack "liftA2")

discardLeftName = VariableSymbol $ variableSymbol (pack "*>")

discardRightName = VariableSymbol $ variableSymbol (pack "<*")

name = \case
  Method.Pure -> pureName
  Method.Ap -> apName
  Method.LiftA2 -> liftA2Name
  Method.DiscardLeft -> discardLeftName
  Method.DiscardRight -> discardRightName

methods = Map.fromList $ do
  enum <- [minBound .. maxBound]
  Prelude.pure (name enum, Prelude.fromEnum enum)

fields = Map.keysSet methods

pure =
  ( pureName,
    Term.Binding
      { position = Position.internal,
        index = Term2.Method Method.pure,
        fixity = Fixity {associativity = Left, precedence = 9},
        selector = Normal
      }
  )

ap =
  ( apName,
    Term.Binding
      { position = Position.internal,
        index = Term2.Method Method.ap,
        fixity = Fixity {associativity = Left, precedence = 4},
        selector = Normal
      }
  )

liftA2 =
  ( liftA2Name,
    Term.Binding
      { position = Position.internal,
        index = Term2.Method Method.liftA2,
        fixity = Fixity {associativity = Left, precedence = 9},
        selector = Normal
      }
  )

discardLeft =
  ( discardLeftName,
    Term.Binding
      { position = Position.internal,
        index = Term2.Method Method.discardLeft,
        fixity = Fixity {associativity = Left, precedence = 4},
        selector = Normal
      }
  )

discardRight =
  ( discardRightName,
    Term.Binding
      { position = Position.internal,
        index = Term2.Method Method.discardRight,
        fixity = Fixity {associativity = Left, precedence = 4},
        selector = Normal
      }
  )

applicative =
  ( applicativeName,
    Type.Binding
      { position = Position.internal,
        index = Type3.Index Type2.Applicative,
        methods,
        fields,
        constructors = Set.empty
      }
  )
