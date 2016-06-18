module Mpl.Core where

import Data.Text (Text)

data Core a = CInt   a Integer
            | CIdent a Text
            | CThunk a (Core a)
            | CForce a (Core a)
            | CFunc  a Text (Core a)
            | CApp   a (Core a) (Core a)
            deriving (Show, Eq, Ord)

meta (CInt    a _)   = a
meta (CIdent  a _)   = a
meta (CThunk  a _)   = a
meta (CForce  a _)   = a
meta (CFunc   a _ _) = a
meta (CApp    a _ _) = a
