module Semantic.Tree.Statements where

data Syntax
  = Guard
  | Do
  | Comprehension

type Guard = 'Guard

type Do = 'Do

type Comprehension = 'Comprehension
