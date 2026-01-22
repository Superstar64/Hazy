module Stage3.Synonym (Context, local, lookup, fromProper) where

import Control.Monad.ST (ST)
import qualified Data.Strict.Maybe as Strict
import qualified Stage2.Index.Table.Type as Type (Map (Map), Table, map, (!))
import qualified Stage2.Index.Table.Type as Type.Table (Table (..))
import qualified Stage2.Index.Type2 as Type2
import Stage2.Scope (Environment ((:+)), Local)
import Stage2.Shift (Shift, shift)
import qualified Stage2.Shift as Shift
import {-# SOURCE #-} qualified Stage3.Check.Context as Proper (Context, typeEnvironment_)
import {-# SOURCE #-} qualified Stage3.Check.TypeBinding as Proper (TypeBinding, synonym_)
import Stage4.Tree.Type (Type)
import Prelude hiding (lookup)

newtype Synonym scope = Synonym (Strict.Maybe (Type (Local ':+ scope)))

instance Shift Synonym where
  shift (Synonym typex) = Synonym (fmap (Shift.map (Shift.Over Shift.Shift)) typex)

newtype TypeBinding s scope = TypeBinding (ST s (Synonym scope))

instance Shift (TypeBinding s) where
  shift (TypeBinding action) = TypeBinding (fmap shift action)

newtype Context s scope = Context (Type.Table (TypeBinding s) scope)

local :: Context s scope -> Context s (Local ':+ scope)
local (Context table) = Context (Type.Table.Local table)

lookup :: Context s scope -> Type2.Index scope -> ST s (Strict.Maybe (Type (Local ':+ scope)))
lookup (Context table) (Type2.Index index) = case table Type.! index of
  TypeBinding action -> do
    Synonym typex <- action
    pure typex
lookup _ _ = pure Strict.Nothing

fromProper :: Proper.Context s scope -> Context s scope
fromProper context = Context $ Type.map (Type.Map fromProperSynonym) $ Proper.typeEnvironment_ context

fromProperSynonym :: Proper.TypeBinding s scope -> TypeBinding s scope
fromProperSynonym proper = TypeBinding $ do
  synonym <- Proper.synonym_ proper
  pure (Synonym synonym)
