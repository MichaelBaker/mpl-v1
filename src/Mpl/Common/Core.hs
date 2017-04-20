module Mpl.Common.Core where

import Mpl.Utils (Text, Generic, Cofree ((:<)), Base, envcata, annotation)
import qualified Mpl.Common.Syntax as S

data CoreF binder recurse
  = Literal Literal
  | Symbol      Text
  | Function    binder recurse
  | Application recurse recurse
  deriving (Show, Generic, Functor, Eq, Traversable, Foldable)

data Literal
  = IntegerLiteral Integer
  deriving (Show, Generic, Eq)

data Binder recurse
  = Binder Text
  deriving (Show, Generic, Functor, Eq, Traversable, Foldable)

literal                    = Literal
symbol                     = Symbol
function parameter body    = Function parameter body
application func argument  = Application func argument
int                        = literal . IntegerLiteral

binder = Binder

syntaxToCore span (S.Literal literal) =
  span :< Literal (convertLiteral literal)
syntaxToCore span (S.Symbol  text) =
  span :< Symbol text
syntaxToCore span (S.Function binders body) =
  curryFunction binders body
syntaxToCore span (S.Application fun args) =
  curryApplication args fun

convertLiteral (S.IntegerLiteral integer) = IntegerLiteral integer

convertBinder span (S.Binder text) = span :< Binder text

-- TODO: Makes span annotations correct
curryFunction [] _ =
  error "Cannot have a function with no parameters" -- TODO: Fold this into monadic error handling
curryFunction (p:[]) body =
  annotation p :< Function (envcata convertBinder p) body
curryFunction (p:ps) body =
  annotation p :< Function (envcata convertBinder p) (curryFunction ps body)

-- TODO: Makes span annotations correct
curryApplication [] _ =
  error "Cannot have an application with no arguments" -- TODO: Fold this into monadic error handling
curryApplication (a:[]) fun =
  annotation a :< Application fun a
curryApplication (a:as) fun =
  curryApplication as (annotation a :< Application fun a)

mapBinder :: (a -> binder) -> CoreF a recurse -> CoreF binder recurse
mapBinder f (Function binder recurse) =
  Function
    (f binder)
    recurse
mapBinder _ (Literal literal)      = Literal literal
mapBinder _ (Symbol      text)     = Symbol text
mapBinder _ (Application fun arg)  = Application fun arg
