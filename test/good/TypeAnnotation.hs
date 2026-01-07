module TypeAnnotation where

type Annotated :: *
data Annotated

ignore :: Annotated -> ()
ignore _ = ()
