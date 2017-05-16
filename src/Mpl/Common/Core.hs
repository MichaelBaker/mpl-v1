module Mpl.Common.Core where

import           Mpl.ParserUtils
import           Mpl.Prelude
import           Mpl.Utils
import qualified Mpl.Common.Syntax as S
import qualified Prelude

data CoreF binder recurse
  = Literal     Literal
  | Symbol      Text
  | Function    binder recurse
  | Application recurse recurse
  deriving (Show, Generic, Functor, Eq, Traversable, Prelude.Foldable)

data Literal
  = IntegerLiteral     Integer
  | UTF8StringLiteral  Text
  deriving (Show, Generic, Eq)

data Binder recurse
  = Binder Text
  deriving (Show, Generic, Functor, Eq, Traversable, Prelude.Foldable)

literal                    = Literal
symbol                     = Symbol
function parameter body    = Function parameter body
application func argument  = Application func argument
int                        = literal . IntegerLiteral
utf8String                 = literal . UTF8StringLiteral

binder = Binder

mapBinder :: (a -> binder) -> CoreF a recurse -> CoreF binder recurse
mapBinder f (Function binder recurse) =
  Function
    (f binder)
    recurse
mapBinder _ (Literal literal)      = Literal literal
mapBinder _ (Symbol      text)     = Symbol text
mapBinder _ (Application fun arg)  = Application fun arg
