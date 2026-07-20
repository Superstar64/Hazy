{-# LANGUAGE RoleAnnotations #-}

module Semantic.Tree.Declarations where

import Data.Kind (Type)
import Semantic.Connect (Connect)
import Semantic.FreeVariables (FreeTermVariables)
import Semantic.Layout (Layout)
import Semantic.Locality (Locality)
import qualified Semantic.Locality as Locality
import Semantic.Scope (Environment (..))
import qualified Semantic.Scope as Scope
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Semantic.Stage (Stage)

type Declarations :: Locality -> Layout -> Stage -> Environment -> Type

type role Declarations nominal nominal nominal nominal

data Declarations locality layout stage scope

newtype Local layout stage scope
  = Local (Declarations Locality.Local layout stage (Scope.Declaration ':+ scope))

instance Show (Local layout stage scope)

instance Shift0.Functor (Local layout stage)

instance Shift.Functor (Local layout stage)

instance FreeTermVariables (Local layout)

instance Connect Local
