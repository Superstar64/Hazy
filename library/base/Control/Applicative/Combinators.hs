{-# LANGUAGE Haskell2010 #-}

{-# LANGUAGE_HAZY StableImports #-}
module Control.Applicative.Combinators
  ( (<|>),
    many,
    some,
    optional,
    empty,
    between,
    choice,
    count,
    count,
    eitherP,
    endBy,
    endBy1,
    manyTill,
    manyTill_,
    someTill,
    someTill_,
    option,
    sepBy,
    sepBy1,
    sepEndBy,
    sepEndBy1,
    skipMany,
    skipSome,
    skipCount,
    skipManyTill,
    skipSomeTill,
  )
where

import Control.Applicative (Alternative (empty, many, some, (<|>)), optional)

between :: (Applicative m) => m open -> m close -> m a -> m a
between = error "todo"

choice :: (Foldable f, Alternative m) => f (m a) -> m a
choice = error "todo"

count :: (Applicative m) => Int -> m a -> m [a]
count = error "todo"

count' :: (Alternative m) => Int -> Int -> m a -> m [a]
count' = error "todo"

eitherP :: (Alternative m) => m a -> m b -> m (Either a b)
eitherP = error "todo"

endBy :: (Alternative m) => m a -> m sep -> m [a]
endBy = error "todo"

endBy1 :: (Alternative m) => m a -> m sep -> m [a]
endBy1 = error "todo"

manyTill :: (Alternative m) => m a -> m end -> m [a]
manyTill = error "todo"

manyTill_ :: (Alternative m) => m a -> m end -> m ([a], end)
manyTill_ = error "todo"

someTill :: (Alternative m) => m a -> m end -> m [a]
someTill = error "todo"

someTill_ :: (Alternative m) => m a -> m end -> m ([a], end)
someTill_ = error "todo"

option :: (Alternative m) => a -> m a -> m a
option = error "todo"

sepBy :: (Alternative m) => m a -> m sep -> m [a]
sepBy = error "todo"

sepBy1 :: (Alternative m) => m a -> m sep -> m [a]
sepBy1 = error "todo"

sepEndBy :: (Alternative m) => m a -> m sep -> m [a]
sepEndBy = error "todo"

sepEndBy1 :: (Alternative m) => m a -> m sep -> m [a]
sepEndBy1 = error "todo"

skipMany :: (Alternative m) => m a -> m ()
skipMany = error "todo"

skipSome :: (Alternative m) => m a -> m ()
skipSome = error "todo"

skipCount :: (Applicative m) => Int -> m a -> m ()
skipCount = error "todo"

skipManyTill :: (Alternative m) => m a -> m end -> m end
skipManyTill = error "todo"

skipSomeTill :: (Alternative m) => m a -> m end -> m end
skipSomeTill = error "todo"
