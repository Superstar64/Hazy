{-# LANGUAGE Haskell2010 #-}

{-# LANGUAGE_HAZY StableImports #-}
module Control.Applicative.Combinators.NonEmpty
  ( some,
    endBy1,
    someTill,
    sepBy1,
    sepEndBy1,
  )
where

import Control.Applicative (Alternative)
import Data.List.NonEmpty (NonEmpty)

some :: (Alternative m) => m a -> m (NonEmpty a)
some = error "todo"

endBy1 :: (Alternative m) => m a -> m sep -> m (NonEmpty a)
endBy1 = error "todo"

someTill :: (Alternative m) => m a -> m end -> m (NonEmpty a)
someTill = error "todo"

sepBy1 :: (Alternative m) => m a -> m sep -> m (NonEmpty a)
sepBy1 = error "todo"

sepEndBy1 :: (Alternative m) => m a -> m sep -> m (NonEmpty a)
sepEndBy1 = error "todo"
