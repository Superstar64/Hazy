module Semantic.Resolve.Builtin.Integral where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (pack)
import qualified Semantic.Index.Method as Method
import qualified Semantic.Index.Term2 as Term2
import qualified Semantic.Index.Type2 as Type2
import qualified Semantic.Index.Type3 as Type3
import Semantic.Resolve.Binding.Term (Selector (Normal))
import qualified Semantic.Resolve.Binding.Term as Term
import qualified Semantic.Resolve.Binding.Type as Type
import Syntax.Lexer (constructorIdentifier, variableIdentifier)
import qualified Syntax.Position as Position
import Syntax.Tree.Associativity (Associativity (..))
import Syntax.Tree.Fixity (Fixity (..))
import Syntax.Variable (Variable (VariableIdentifier))
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
