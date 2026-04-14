module Stage2.FreeVariables where

import {-# SOURCE #-} qualified Stage2.Index.Term0 as Term0
import {-# SOURCE #-} qualified Stage2.Index.Type0 as Type0
import Stage2.Scope (Environment (..))

data Target scope scope' where
  Target :: Target scope scope
  Over :: Target scopes scopes' -> Target (scope ':+ scopes) scopes'

class FreeTermVariables expression where
  freeTermVariables :: Target scope scope' -> expression scope -> [Term0.Index scope']

class FreeTypeVariables typex where
  freeTypeVariables :: Target scope scope' -> typex scope -> [Type0.Index scope']
