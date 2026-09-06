{-# LANGUAGE RoleAnnotations #-}

module Semantic.Tree.Declarations where

import Data.Functor.Identity (Identity)
import Data.Kind (Type)
import Data.Void (Void)
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

type Declarations :: (Type -> Type) -> Type -> Locality -> (Type -> Type) -> Layout -> Stage -> Environment -> Type

type role Declarations nominal nominal nominal representational nominal nominal nominal

data Declarations solve logical locality loeb layout stage scope

newtype Local layout stage scope
  = Local (Declarations Identity Void Locality.Local Identity layout stage (Scope.Declaration ':+ scope))

instance Show (Local layout stage scope)

instance Shift0.Functor (Local layout stage)

instance Shift.Functor (Local layout stage)

instance FreeTermVariables Local

instance Connect Local
