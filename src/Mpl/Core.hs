module Mpl.Core where

import Data.Text (Text)

data Core m
  = CInt   Path m Integer
  | CIdent Path m Text
  | CThunk Path m (Core m)
  | CForce Path m (Core m)
  | CFunc  Path m Param (Core m)
  | CApp   Path m (Core m) (Core m)
  deriving (Show, Eq)

type Param = Text
type Path  = [Int]

data Type
  = TUnknown
  | TUnboundIdent
  | TPoly
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

pathOf (CInt   path _ _)   = path
pathOf (CIdent path _ _)   = path
pathOf (CThunk path _ _)   = path
pathOf (CForce path _ _)   = path
pathOf (CFunc  path _ _ _) = path
pathOf (CApp   path _ _ _) = path
