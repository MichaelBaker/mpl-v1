module Mpl.Untyped.Syntax where

-- A dynamic lanugage with as few compile-time restrictions as possible.
-- For experienced programmers, this should provide a language in which to do fast scripting and experimentation.
-- For beginning programmers, it should provide a fertile learning environment where there are relatively few concepts to learn.

import Mpl.Utils (Generic)

import qualified Mpl.Common.Syntax as CS

data SyntaxF r = Common (CS.SyntaxF r) deriving (Show, Generic, Functor, Eq, Traversable, Foldable)

literal                    = Common . CS.literal
symbol                     = Common . CS.symbol
function parameters body   = Common $ CS.function parameters body
application func arguments = Common $ CS.application func arguments
leftAssociative            = Common . CS.leftAssociative
rightAssociative           = Common . CS.rightAssociative
int                        = Common . CS.int
