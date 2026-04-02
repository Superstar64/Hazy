module Stage3.Check.Mask where

-- |
-- A mask for rigid type variables
data Mask
  = -- | Unknown at runtime
    Runtime
  | -- | Delayed knowledge
    Inline

-- |
-- A desire for a unification variable
data Erasure
  = -- | This type is erased and not needed at runtime
    Erased
  | -- | This type must be know at runtime
    Known

instance Semigroup Erasure where
  Known <> _ = Known
  Erased <> x = x

valid :: Mask -> Erasure -> Bool
valid _ Erased = True
valid Inline Known = True
valid _ _ = False
