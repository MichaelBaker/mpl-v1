module Mpl.Core where

import Data.Text (Text)

data Core
  = CInt   [Int] Type Integer
  | CIdent [Int] Type Text
  | CThunk [Int] Type Core
  | CForce [Int] Type Core
  | CFunc  [Int] Type Param Core
  | CApp   [Int] Type Core Core
  deriving (Show, Eq)

type Param = Text

data Type
  = TUnknown
  | TInt
  | TThunk Type
  | TFunc Type Type
  deriving (Show, Eq)