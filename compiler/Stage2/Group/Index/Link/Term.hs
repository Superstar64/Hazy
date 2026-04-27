module Stage2.Group.Index.Link.Term where

import qualified Stage2.Group.Index.Term0 as Term0
import qualified Stage2.Index.Link.Term as Proper
import qualified Stage2.Locality as Locality
import qualified Stage2.Scope as Scope

data Link locality
  = Link !(Proper.Link locality)
  | Share !(Proper.Link locality)
  deriving (Show, Eq, Ord)

global :: Term0.Index Scope.Global -> Link Locality.Global
global (Term0.Index link) = Link $ Proper.global link
global (Term0.Share link) = Share $ Proper.global link

local :: Term0.Index (Scope.Declaration 'Scope.:+ scope) -> Link Locality.Local
local (Term0.Index link) = Link $ Proper.local link
local (Term0.Share link) = Share $ Proper.local link
