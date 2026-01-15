module Stage4.Tree.Builtin where

import qualified Stage2.Index.Constructor as Constructor
import qualified Stage2.Index.Type as Type
import qualified Stage2.Index.Type2 as Type2
import {-# SOURCE #-} Stage4.Tree.Class (Class)
import {-# SOURCE #-} Stage4.Tree.Data (Data)
import Stage4.Tree.Type (Type)

kind ::
  (Type scope -> typex) ->
  (Type.Index scope -> typex) ->
  (Constructor.Index scope -> typex) ->
  Type2.Index scope ->
  typex

class Builtin builtin where
  index ::
    (builtin scope -> target) ->
    (Type.Index scope -> target) ->
    Type2.Index scope ->
    target

instance Builtin Data

instance Builtin Class
