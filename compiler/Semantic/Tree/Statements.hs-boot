module Semantic.Tree.Statements (Syntax (..), Guard, Do) where

data Syntax
  = Guard
  | Do

type Guard = 'Guard

type Do = 'Do
