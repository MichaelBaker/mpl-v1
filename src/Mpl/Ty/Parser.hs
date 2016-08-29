module Mpl.Ty.Parser where

import Mpl.Ty.AST (AST(..), TyAST(..))

import Control.Applicative       ((<|>), many, some)
import Data.Text                 (pack)
import Text.Parser.Char          (oneOf)
import Text.Parser.Token         (whiteSpace, symbolic, parens)
import Text.Parser.Combinators   ((<?>), try, optional)

import qualified Mpl.Parse      as Parse
import qualified Mpl.Dyn.Parser as Dyn

data ParseType = Exp

parseFile :: Parse.FileParser ParseType AST
parseFile Exp filepath = Parse.parseFromFile expressionWithApp filepath

parseString :: Parse.StringParser ParseType AST
parseString Exp string = Parse.parseFromString expressionWithApp Parse.zeroDelta string

maybeAnnotateExpression annotatableExpressions = (do
  startSpan <- Parse.getPosition
  dynExp    <- annotatableExpressions
  maybeTy   <- optional annotation
  case maybeTy of
    Nothing -> return dynExp
    Just ty -> do
      endSpan <- Parse.getPosition
      return $ AAnnExp dynExp ty (Parse.makeSpan startSpan endSpan)
  ) <?> "expression"

annotation = symbolic ':' *> whiteSpace *> type_ <?> "type annotation"

expression = maybeAnnotateExpression
   (  try lambda
  <|> parens expressionWithApp
  <|> Dyn.makeLetExp ALet definition expression
  <|> (ADyn <$> Dyn.utf16)
  <|> lens
  <|> (ADyn <$> try Dyn.real)
  <|> (ADyn <$> Dyn.int)
  <|> Dyn.record ARec (Dyn.recordField AField expression)
  <|> Dyn.list AList expression
  <|> (ADyn <$> Dyn.symbol)
   )

definition        = Dyn.makeDefinition ADef ALam binding
binding           = Dyn.makeBinding expression
lambda            = Dyn.makeLambda ALam binding expression
expressionWithApp = maybeAnnotateExpression $ Dyn.makeExpressionWithApp AApp ALensApp expression lens
lens              = Dyn.makeLens ALens ADyn expression

type_ = Parse.withSpan $ (do
  firstChar <- oneOf tyStartChars <?> "start of type"
  rest      <- (many $ oneOf tyChars) <?> "tail of type"
  whiteSpace
  let name = firstChar : rest
  return $ ATySym $ pack name) <?> "type"

tyStartChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
tyChars      = "abcdefghijklmnopqrstuvwxyz" ++ tyStartChars ++ digits
digits       = "0123456789"
