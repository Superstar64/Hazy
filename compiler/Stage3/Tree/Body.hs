module Stage3.Tree.Body where

import qualified Data.Strict.Vector1 as Strict
import {-# SOURCE #-} Stage3.Tree.Expression (Expression)
import {-# SOURCE #-} Stage3.Tree.Statements (Statements)

data Body scope
  = Body {body :: !(Expression scope)}
  | Guards {guards :: !(Strict.Vector1 (Statements scope))}
  deriving (Show)
