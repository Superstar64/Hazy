module Stage2.Resolve.Builtin.Enum
  ( succ,
    pred,
    toEnum,
    fromEnum,
    enumFrom,
    enumFromThen,
    enumFromTo,
    enumFromThenTo,
    enum,
  )
where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (pack)
import Stage1.Lexer (constructorIdentifier, variableIdentifier)
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
import Prelude hiding (Either (..), Enum (..))
import qualified Prelude

enumName = constructorIdentifier (pack "Enum")

succName = VariableIdentifier $ variableIdentifier (pack "succ")

predName = VariableIdentifier $ variableIdentifier (pack "pred")

toEnumName = VariableIdentifier $ variableIdentifier (pack "toEnum")

fromEnumName = VariableIdentifier $ variableIdentifier (pack "fromEnum")

enumFromName = VariableIdentifier $ variableIdentifier (pack "enumFrom")

enumFromThenName = VariableIdentifier $ variableIdentifier (pack "enumFromThen")

enumFromToName = VariableIdentifier $ variableIdentifier (pack "enumFromTo")

enumFromThenToName = VariableIdentifier $ variableIdentifier (pack "enumFromThenTo")

name = \case
  Method.Succ -> succName
  Method.Pred -> predName
  Method.ToEnum -> toEnumName
  Method.FromEnum -> fromEnumName
  Method.EnumFrom -> enumFromName
  Method.EnumFromThen -> enumFromThenName
  Method.EnumFromTo -> enumFromToName
  Method.EnumFromThenTo -> enumFromThenToName

methods = Map.fromList $ do
  enum <- [minBound .. maxBound]
  pure (name enum, Prelude.fromEnum enum)

fields = Map.keysSet methods

succ =
  ( succName,
    Term.Binding
      { position = Position.internal,
        index = Term2.Method Method.succ,
        fixity = Fixity {associativity = Left, precedence = 9},
        selector = Normal
      }
  )

pred =
  ( predName,
    Term.Binding
      { position = Position.internal,
        index = Term2.Method Method.pred,
        fixity = Fixity {associativity = Left, precedence = 9},
        selector = Normal
      }
  )

toEnum =
  ( toEnumName,
    Term.Binding
      { position = Position.internal,
        index = Term2.Method Method.toEnum,
        fixity = Fixity {associativity = Left, precedence = 9},
        selector = Normal
      }
  )

fromEnum =
  ( fromEnumName,
    Term.Binding
      { position = Position.internal,
        index = Term2.Method Method.fromEnum,
        fixity = Fixity {associativity = Left, precedence = 9},
        selector = Normal
      }
  )

enumFrom =
  ( enumFromName,
    Term.Binding
      { position = Position.internal,
        index = Term2.Method Method.enumFrom,
        fixity = Fixity {associativity = Left, precedence = 9},
        selector = Normal
      }
  )

enumFromThen =
  ( enumFromThenName,
    Term.Binding
      { position = Position.internal,
        index = Term2.Method Method.enumFromThen,
        fixity = Fixity {associativity = Left, precedence = 9},
        selector = Normal
      }
  )

enumFromTo =
  ( enumFromToName,
    Term.Binding
      { position = Position.internal,
        index = Term2.Method Method.enumFromTo,
        fixity = Fixity {associativity = Left, precedence = 9},
        selector = Normal
      }
  )

enumFromThenTo =
  ( enumFromThenToName,
    Term.Binding
      { position = Position.internal,
        index = Term2.Method Method.enumFromThenTo,
        fixity = Fixity {associativity = Left, precedence = 9},
        selector = Normal
      }
  )

enum =
  ( enumName,
    Type.Binding
      { position = Position.internal,
        index = Type3.Index Type2.Enum,
        constructors = Set.empty,
        fields,
        methods
      }
  )
