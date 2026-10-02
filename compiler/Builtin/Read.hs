module Builtin.Read where

import Builtin (builtinClass)
import Core.Tree.Class (Class)
import Core.Tree.ClassExtra (ClassExtra (..))
import Data.Text (pack)
import qualified Semantic.Index.Type2 as Type2
import Semantic.Resolve.Bindings (Bindings)

-- This `readList` implementation is slightly wrong. The `minilex` helper is
-- not unicode aware. This implementation assumes that `readsPrec` never tries
-- to parse bare ']'.

bindings :: Bindings () scope
definition :: Class scope
extra :: ClassExtra scope
(bindings, definition, extra) =
  Type2.Read
    `builtinClass` pack
      """
      module X where
      import {-# BUILTIN #-} Hazy

      class Read a where
        readsPrec :: Int -> [Char] -> [(a, [Char])]
        readList :: [Char] -> [([a], [Char])]
        readList = run
          where
            minilex (c : s) = case c of
              ' ' -> minilex s
              '\\t' -> minilex s
              '\\n' -> minilex s
              '\\r' -> minilex s
              '\\f' -> minilex s
              '\\v' -> minilex s
              _ -> [(c, s)]
            minilex "" = []
            run r =
              do
                (l, s) <- minilex r
                case l of
                  '[' -> case minilex s of
                    [(']', t)] -> [([], t)]
                    _ -> readElement s
                  '(' -> do
                    (x, t) <- run s
                    (')', u) <- minilex t
                    [(x, u)]
                  _ -> []
            readElement t = do
              (x, u) <- readsPrec 0 t
              (xs, v) <- readTail u
              [(x : xs, v)]
            readTail s = do
              (l, t) <- minilex s
              case l of
                ']' -> [([], t)]
                ',' -> readElement t
                _ -> []
      """
