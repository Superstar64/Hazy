module Unique where

import qualified Data.Set as Set

unique :: (Ord a) => [a] -> Bool
unique = go Set.empty
  where
    go known = \case
      [] -> True
      (head : tail)
        | Set.member head known -> False
        | otherwise -> go (Set.insert head known) tail
