module Mpl.Untyped.Syntax where

import           Mpl.Prelude
import qualified Mpl.Common.Syntax as CS
import qualified Prelude

data SyntaxF binder recurse
  = Common (CS.SyntaxF binder recurse)
  deriving (Show, Generic, Functor, Eq, Traversable, Prelude.Foldable)

symbol                     = Common . CS.symbol
function parameters body   = Common $ CS.function parameters body
application func arguments = Common $ CS.application func arguments
int                        = Common . CS.int
utf8String                 = Common . CS.utf8String

binder                     = CS.binder

mapCommon f (Common a) = Common (f a)
