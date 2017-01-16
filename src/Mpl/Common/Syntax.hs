module Mpl.Common.Syntax where

import Mpl.Utils as Utils (Text, Generic, Fix(..), Base, cata)

data SyntaxF binder recurse
  = Literal Literal
  | Symbol      Text
  | Function    [binder] recurse
  | Application recurse [recurse]
  deriving (Show, Generic, Functor, Eq, Traversable, Foldable)

data Literal =
  IntegerLiteral Integer
  deriving (Show, Generic, Eq)

data Binder r =
  Binder Text
  deriving (Show, Generic, Functor, Eq, Traversable, Foldable)

literal                    = Literal
symbol                     = Symbol
function parameters body   = Function parameters body
application func arguments = Application func arguments
int                        = literal . IntegerLiteral

binder                     = Binder

mapBinder :: (a -> binder) -> SyntaxF a recurse -> SyntaxF binder recurse
mapBinder f (Function binders recurse) =
  Function
    (map f binders)
    recurse
mapBinder _ (Literal literal) = Literal literal
mapBinder _ (Symbol      text) = Symbol text
mapBinder _ (Application fun args) = Application fun args
