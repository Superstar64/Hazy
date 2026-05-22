module Stage2.FreeVariables where

import Control.Applicative (Const (..))
import qualified Stage2.Index.Term as Term
import qualified Stage2.Index.Term0 as Term0
import qualified Stage2.Index.Type as Type
import qualified Stage2.Index.Type0 as Type0
import qualified Stage2.Index.Type2 as Type2
import Stage2.Scope (Environment (..))
import Stage2.Stage (Resolve)

data Target scope scope' where
  Target :: Target scope scope
  Over :: Target scopes scopes' -> Target (scope ':+ scopes) scopes'

class FreeTermVariables expression where
  freeTermVariables :: Target scope scope' -> expression Resolve scope -> [Term0.Index scope']

term :: Target scope scope' -> Term.Index scope -> [Term0.Index scope']
term Target (Term.Declaration index) = [Term0.Declaration index]
term Target (Term.Global global local) = [Term0.Global global local]
term (Over free) (Term.Shift index) = term free index
term _ _ = []

class FreeTypeVariables typex where
  freeTypeVariables :: Target scope scope' -> typex Resolve scope -> [Type0.Index scope']

typex :: Target scope scope' -> Type.Index scope -> [Type0.Index scope']
typex Target (Type.Declaration index) = [Type0.Declaration index]
typex Target (Type.Global global local) = [Type0.Global global local]
typex (Over free) (Type.Shift index) = typex free index
typex _ _ = []

type2 :: Target scope scope' -> Type2.Index scope -> [Type0.Index scope']
type2 target index = getConst $ Type2.traverse (Const . typex target) index
