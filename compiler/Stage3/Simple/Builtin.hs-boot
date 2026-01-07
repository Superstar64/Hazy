module Stage3.Simple.Builtin where

import qualified Stage2.Index.Constructor as Constructor
import qualified Stage2.Index.Type as Type
import qualified Stage2.Index.Type2 as Type2
import {-# SOURCE #-} Stage3.Simple.Class (Class)
import {-# SOURCE #-} Stage3.Simple.Data (Data)
import {-# SOURCE #-} Stage3.Simple.Type (Type)

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
