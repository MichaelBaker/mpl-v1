module Mpl.Core where

import Data.Text (Text)

data Core
  = CInt   [Int] Integer
  | CIdent [Int] Text
  | CThunk [Int] Core
  | CForce [Int] Core
  | CFunc  [Int] Text Core
  | CApp   [Int] Core Core
  deriving (Show, Eq, Ord)
