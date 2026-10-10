module Core.Instanciate where

data Instanciate = Normal | Instanciated

type Normal = 'Normal

type Instanciated = 'Instanciated

data Store instanciate a where
  Empty :: Store 'Normal a
  Store :: !a -> Store 'Instanciated a

instance (Show a) => Show (Store instanciate a) where
  showsPrec k = \case
    Empty -> showString "Empty"
    Store a -> showParen (k > 10) $ showString "Store " . showsPrec 11 a
