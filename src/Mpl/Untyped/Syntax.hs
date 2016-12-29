module Mpl.Untyped.Syntax where

import Mpl.Utils (Generic)

import qualified Mpl.Common.Syntax as CS

data SyntaxF recurse
  = Common (CS.SyntaxF recurse)
  deriving (Show, Generic, Functor, Eq, Traversable, Foldable)

literal                    = Common . CS.literal
binder                     = Common . CS.binder
symbol                     = Common . CS.symbol
function parameters body   = Common $ CS.function parameters body
application func arguments = Common $ CS.application func arguments
int                        = Common . CS.int
