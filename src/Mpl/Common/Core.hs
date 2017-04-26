module Mpl.Common.Core where

import           Mpl.ParserUtils   (SourceAnnotated)
import           Mpl.Prelude
import           Mpl.Utils
import qualified Mpl.Common.Syntax as S
import qualified Prelude

data CoreF binder recurse
  = Literal Literal
  | Symbol      Text
  | Function    binder recurse
  | Application recurse recurse
  deriving (Show, Generic, Functor, Eq, Traversable, Prelude.Foldable)

data Literal
  = IntegerLiteral Integer
  deriving (Show, Generic, Eq)

data Binder recurse
  = Binder Text
  deriving (Show, Generic, Functor, Eq, Traversable, Prelude.Foldable)

type SourceBinder = SourceAnnotated Binder

literal                    = Literal
symbol                     = Symbol
function parameter body    = Function parameter body
application func argument  = Application func argument
int                        = literal . IntegerLiteral

binder = Binder

syntaxToCore wrap span (S.Literal literal) =
  span :< (wrap $ Literal (convertLiteral literal))
syntaxToCore wrap span (S.Symbol  text) =
  span :< (wrap $ Symbol text)
syntaxToCore wrap span (S.Function binders body) =
  curryFunction wrap binders body
syntaxToCore wrap span (S.Application fun args) =
  curryApplication wrap args fun

convertLiteral (S.IntegerLiteral integer) = IntegerLiteral integer

convertBinder span (S.Binder text) = span :< Binder text

-- TODO: Makes span annotations correct
curryFunction _ [] _ =
  error "Cannot have a function with no parameters" -- TODO: Fold this into monadic error handling
curryFunction wrap (p:[]) body =
  annotation p :< (wrap $ Function (envcata convertBinder p) body)
curryFunction wrap (p:ps) body =
  annotation p :< (wrap $ Function (envcata convertBinder p) (curryFunction wrap ps body))

-- TODO: Makes span annotations correct
curryApplication _ [] _ =
  error "Cannot have an application with no arguments" -- TODO: Fold this into monadic error handling
curryApplication wrap (a:[]) fun =
  annotation a :< (wrap $ Application fun a)
curryApplication wrap (a:as) fun =
  curryApplication wrap as (annotation a :< (wrap $ Application fun a))

mapBinder :: (a -> binder) -> CoreF a recurse -> CoreF binder recurse
mapBinder f (Function binder recurse) =
  Function
    (f binder)
    recurse
mapBinder _ (Literal literal)      = Literal literal
mapBinder _ (Symbol      text)     = Symbol text
mapBinder _ (Application fun arg)  = Application fun arg
