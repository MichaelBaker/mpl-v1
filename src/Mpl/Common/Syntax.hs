module Mpl.Common.Syntax where

import Mpl.Utils (Text, Generic, Fix(..))

data SyntaxF recurse
  = Literal Literal
  | Symbol Text
  | Binder Text
  | Function [recurse]
             recurse
  | Application recurse
                [recurse]
  deriving (Show, Generic, Functor, Eq, Traversable, Foldable)

data Literal =
  IntegerLiteral Integer
  deriving (Show, Generic, Eq)

literal                    = Literal
binder                     = Binder
symbol                     = Symbol
function parameters body   = Function parameters body
application func arguments = Application func arguments
int                        = literal . IntegerLiteral
