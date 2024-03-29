module Mpl.Common.BackendJS where

import Data.Functor.Foldable               (cata)
import Data.List                           (foldl')
import Data.Text                           (unpack)
import Language.JavaScript.Parser
import Language.JavaScript.Parser.AST
import Language.JavaScript.Parser.Grammar5
import Language.JavaScript.Parser.Lexer
import Mpl.Common.Core
import Mpl.JSUtils
import Mpl.Prelude
import Mpl.Utils

translate _ (Literal l) =
  return $ translateLiteral l

translate _ (Symbol t) = do
  maybeJSBinder <- findBinder t
  case maybeJSBinder of
    Just jsBinder ->
      return $ JSIdentifier JSNoAnnot (unpack jsBinder)
    Nothing -> do
      maybeNative <- getNative t
      case maybeNative of
        Just a  ->
          return a
        Nothing ->
          if isValidIdent t
            then
              return $ JSIdentifier JSNoAnnot (unpack t)
            else do
              sym <- genSym
              return $ JSIdentifier JSNoAnnot (unpack sym)

translate _ (Application f arg) = do
  function <- f
  argument <- arg
  return $
    JSCallExpression
      (JSExpressionParen JSNoAnnot function JSNoAnnot)
      JSNoAnnot
      (JSLOne argument)
      JSNoAnnot

translate translateBinder (Function parameter body) = do
  rawParam <- cata translateBinder parameter
  retValue <- body
  popBinder

  let retStatement =
        JSReturn
          spaceAnnot
          (Just $ addSpaceBefore retValue)
          (JSSemi JSNoAnnot)

  (param, statements) <-
    case rawParam of
      a@(JSIdentifier _ _) ->
        return (jsIdent a, [retStatement])
      other -> do
        uniqIdent <- genSym
        let ident =
              JSIdentifier JSNoAnnot (textToString uniqIdent)
        let setter =
              JSAssignStatement
                (addSpaceBefore other)
                (JSAssign spaceAnnot)
                (addSpaceBefore ident)
                (JSSemi JSNoAnnot)
        return (jsIdent ident, [setter, retStatement])

  return $
    JSFunctionExpression
      JSNoAnnot
      JSIdentNone
      JSNoAnnot
      (JSLOne param)
      JSNoAnnot
      (JSBlock spaceAnnot statements spaceAnnot)

translateBinder (Binder t) = do
  jsBinder <- do
    if isValidIdent t
      then return t
      else genSym
  pushBinder t jsBinder
  return $ JSIdentifier JSNoAnnot (unpack jsBinder)

translateLiteral (IntegerLiteral int) =
  JSDecimal JSNoAnnot (show int)

translateLiteral (UTF8StringLiteral text) =
  JSStringLiteral JSNoAnnot (show text)

isValidIdent ident =
  case runAlex (textToString ident) parseExpression of
    Right (JSAstExpression (JSIdentifier _ _) _) -> True
    _ -> False

jsIdent (JSIdentifier annotation a) = JSIdentName annotation a
jsIdent a = error $ "'" ++ show a ++ "' cannot be converted into an identifier"

spaceAnnot = JSAnnot tokenPosnEmpty [space]
space      = WhiteSpace tokenPosnEmpty " "

addComment (JSIdentifier annot value) comment =
  JSIdentifier (attach annot $ commentAnnot comment) value

addComment (JSCallExpression f ann0 a ann1) comment =
  JSCallExpression f ann0 a (attach ann1 $ commentAnnot comment)

addComment (JSDecimal ann a) comment =
  JSDecimal (attach ann $ commentAnnot comment) a

addComment (JSFunctionExpression ann0 name ann1 params ann2 body) comment =
  JSFunctionExpression ann0 name ann1 params (attach ann2 $ commentAnnot comment) body

addComment js _ =
  error $ "Cannot add a comment to: " ++ show js

commentAnnot string =
  CommentA tokenPosnEmpty (" /* " ++ string ++ " */ ")

addSpaceBefore (JSExpressionParen ann0 value ann1) =
  JSExpressionParen (attach ann0 space) value ann1

addSpaceBefore (JSIdentifier annot value) =
  JSIdentifier (attach annot space) value

addSpaceBefore (JSCallExpression f ann0 a ann1) =
  JSCallExpression (addSpaceBefore f) ann0 a ann1

addSpaceBefore (JSDecimal ann a) =
  JSDecimal (attach ann space) a

addSpaceBefore (JSFunctionExpression ann0 name ann1 params ann2 body) =
  JSFunctionExpression (attach ann0 space) name ann1 params ann2 body

addSpaceBefore (JSCallExpressionSquare receiver ann0 arg ann1) =
  JSCallExpressionSquare (addSpaceBefore receiver) ann0 arg ann1

addSpaceBefore js =
  error $ "Cannot add a space before: " ++ show js

attach JSNoAnnot annotation = JSAnnot tokenPosnEmpty [annotation]
attach (JSAnnot pos annotations) annotation = JSAnnot pos (annotations ++ [annotation])
