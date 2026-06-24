module Core.Builtin where

import {-# SOURCE #-} Core.Tree.Class (Class)
import {-# SOURCE #-} Core.Tree.ClassExtra (ClassExtra)
import {-# SOURCE #-} Core.Tree.Data (Data)
import Core.Tree.Type (Type)
import qualified Semantic.Index.Constructor as Constructor
import qualified Semantic.Index.Type as Type
import qualified Semantic.Index.Type2 as Type2

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

instance Builtin ClassExtra
