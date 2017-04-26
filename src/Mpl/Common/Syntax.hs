module Mpl.Common.Syntax where

import           Mpl.Prelude
import           Mpl.Utils
import qualified Prelude

data SyntaxF binder recurse
  = Literal Literal
  | Symbol      Text
  | Function    [binder] recurse
  | Application recurse [recurse]
  deriving (Show, Generic, Functor, Eq, Traversable, Prelude.Foldable)

data Literal
  = IntegerLiteral Integer
  deriving (Show, Generic, Eq)

data Binder recurse
  = Binder Text
  deriving (Show, Generic, Functor, Eq, Traversable, Prelude.Foldable)

literal                    = Literal
symbol                     = Symbol
function parameters body   = Function parameters body
application func arguments = Application func arguments
int                        = literal . IntegerLiteral

binder = Binder

mapBinder :: (a -> binder) -> SyntaxF a recurse -> SyntaxF binder recurse
mapBinder f (Function binders recurse) =
  Function
    (fmap f binders)
    recurse
mapBinder _ (Literal literal)      = Literal literal
mapBinder _ (Symbol      text)     = Symbol text
mapBinder _ (Application fun args) = Application fun args
