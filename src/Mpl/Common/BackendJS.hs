module Mpl.Common.BackendJS where

import Language.JavaScript.Parser
import Language.JavaScript.Parser.AST
import Mpl.Common.Syntax

import Data.Text (unpack)
import Data.List (foldl')

translate (Literal l) = translateLiteral l
translate (Symbol t) = JSIdentifier JSNoAnnot (unpack t)
translate (Application f as) = curryApplication f as

translateLiteral (IntegerLiteral int) = JSDecimal JSNoAnnot (show int)

curryApplication f as = foldl' curry f as
  where curry f' a = JSCallExpression f' JSNoAnnot (JSLOne a) JSNoAnnot

addComment (JSIdentifier annot value) comment = JSIdentifier (attach annot comment) value
addComment (JSCallExpression f ann0 a ann1) comment = JSCallExpression f ann0 a (attach ann1 comment)
addComment (JSDecimal ann a) comment = JSDecimal (attach ann comment) a
addComment js _ = error $ "Cannot add a comment to: " ++ show js -- TODO: Make this a compiler error

attach JSNoAnnot comment = JSAnnot tokenPosnEmpty [CommentA tokenPosnEmpty (" /* " ++ comment ++ " */ ")]
attach (JSAnnot pos comments) comment = JSAnnot pos (CommentA tokenPosnEmpty (" /* " ++ comment ++ "*/ "):comments)
