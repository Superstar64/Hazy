{-# LANGUAGE RoleAnnotations #-}

module Stage2.Temporary.TypeInfix where

import qualified Data.Kind as Kind (Type)
import Stage1.Position (Position)
import Stage1.Tree.Associativity (Associativity)
import qualified Stage1.Tree.TypeInfix as Stage1 (Infix)
import Stage2.Resolve.Context (Context)
import Stage2.Scope (Environment)
import Stage2.Temporary.Infix (Infix)
import {-# SOURCE #-} Stage2.Tree.Type (Type)

type role Index nominal

type Index :: Environment -> Kind.Type
data Index scope

fixWith :: Maybe Associativity -> Int -> Infix (Index scope) (Type Position scope) -> Type Position scope
fix :: Infix (Index scope) (Type Position scope) -> Type Position scope
resolve :: Context scope -> Stage1.Infix Position -> Infix (Index scope) (Type Position scope)
