module Stage2.Resolve.Builtin.Integral where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (pack)
import Stage1.Lexer (constructorIdentifier, variableIdentifier)
import qualified Stage1.Position as Position
import Stage1.Tree.Associativity (Associativity (..))
import Stage1.Tree.Fixity (Fixity (..))
import Stage1.Variable (Variable (VariableIdentifier))
import qualified Stage2.Index.Method as Method
import qualified Stage2.Index.Term2 as Term2
import qualified Stage2.Index.Type2 as Type2
import qualified Stage2.Index.Type3 as Type3
import Stage2.Resolve.Binding.Term (Selector (Normal))
import qualified Stage2.Resolve.Binding.Term as Term
import qualified Stage2.Resolve.Binding.Type as Type
import Prelude hiding (Either (..))

integralName = constructorIdentifier $ pack "Integral"

quotName = VariableIdentifier $ variableIdentifier $ pack "quot"

remName = VariableIdentifier $ variableIdentifier $ pack "rem"

divName = VariableIdentifier $ variableIdentifier $ pack "div"

modName = VariableIdentifier $ variableIdentifier $ pack "mod"

quotRemName = VariableIdentifier $ variableIdentifier $ pack "quotRem"

divModName = VariableIdentifier $ variableIdentifier $ pack "divMod"

toIntegerName = VariableIdentifier $ variableIdentifier $ pack "toInteger"

name = \case
  Method.Quot -> quotName
  Method.Rem -> remName
  Method.Div -> divName
  Method.Mod -> modName
  Method.QuotRem -> quotRemName
  Method.DivMod -> divModName
  Method.ToInteger -> toIntegerName

methods = Map.fromList $ do
  enum <- [minBound .. maxBound]
  pure (name enum, Prelude.fromEnum enum)

fields = Map.keysSet methods

quot =
  ( quotName,
    Term.Binding
      { position = Position.internal,
        index = Term2.Method Method.quot,
        fixity = Fixity {associativity = Left, precedence = 7},
        selector = Normal
      }
  )

rem =
  ( remName,
    Term.Binding
      { position = Position.internal,
        index = Term2.Method Method.rem,
        fixity = Fixity {associativity = Left, precedence = 7},
        selector = Normal
      }
  )

div =
  ( divName,
    Term.Binding
      { position = Position.internal,
        index = Term2.Method Method.div,
        fixity = Fixity {associativity = Left, precedence = 7},
        selector = Normal
      }
  )

mod =
  ( modName,
    Term.Binding
      { position = Position.internal,
        index = Term2.Method Method.mod,
        fixity = Fixity {associativity = Left, precedence = 7},
        selector = Normal
      }
  )

quotRem =
  ( quotRemName,
    Term.Binding
      { position = Position.internal,
        index = Term2.Method Method.quotRem,
        fixity = Fixity {associativity = Left, precedence = 9},
        selector = Normal
      }
  )

divMod =
  ( divModName,
    Term.Binding
      { position = Position.internal,
        index = Term2.Method Method.divMod,
        fixity = Fixity {associativity = Left, precedence = 9},
        selector = Normal
      }
  )

toInteger =
  ( toIntegerName,
    Term.Binding
      { position = Position.internal,
        index = Term2.Method Method.toInteger,
        fixity = Fixity {associativity = Left, precedence = 9},
        selector = Normal
      }
  )

integral =
  ( integralName,
    Type.Binding
      { position = Position.internal,
        index = Type3.Index Type2.Integral,
        constructors = Set.empty,
        methods,
        fields
      }
  )
