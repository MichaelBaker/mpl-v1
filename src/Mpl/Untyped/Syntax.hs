module Mpl.Untyped.Syntax where

import Mpl.Utils (Generic)

import qualified Mpl.Common.Syntax as CS

data SyntaxF binder recurse
  = Common (CS.SyntaxF binder recurse)
  deriving (Show, Generic, Functor, Eq, Traversable, Foldable)

literal                    = Common . CS.literal
symbol                     = Common . CS.symbol
function parameters body   = Common $ CS.function parameters body
application func arguments = Common $ CS.application func arguments
int                        = Common . CS.int

binder                     = CS.binder

mapCommon f (Common a) = Common (f a)
