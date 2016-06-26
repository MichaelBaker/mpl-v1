module Mpl.Core where

import Data.Text (Text)

data Core m
  = CInt   [Int] m Integer
  | CIdent [Int] m Text
  | CThunk [Int] m (Core m)
  | CForce [Int] m (Core m)
  | CFunc  [Int] m Param (Core m)
  | CApp   [Int] m (Core m) (Core m)
  deriving (Show, Eq)

type Param = Text

data Type
  = TUnknown
  | TInt
  | TThunk Type
  | TFunc Type Type
  deriving (Show, Eq)

metaOf (CInt   _ m _)   = m
metaOf (CIdent _ m _)   = m
metaOf (CThunk _ m _)   = m
metaOf (CForce _ m _)   = m
metaOf (CFunc  _ m _ _) = m
metaOf (CApp   _ m _ _) = m
