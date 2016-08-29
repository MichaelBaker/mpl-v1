module Mpl.Ty.Parser where

import Mpl.Ty.AST (AST(..), TyAST(..))

import Mpl.Span                  (zeroDelta, withSpan, makeSpan, getPosition)
import Control.Applicative       ((<|>), many, some)
import Data.Text                 (pack)
import Text.Parser.Char          (oneOf)
import Text.Parser.Token         (whiteSpace, symbolic, parens)
import Text.Parser.Combinators   ((<?>), try, optional)
import Text.Trifecta.Result      (Result())
import Text.Trifecta.Parser      (parseFromFileEx)

import qualified Mpl.Dyn.Parser as Dyn
import qualified Text.Trifecta.Parser as Parser

data ParseType = Exp

parseFile :: ParseType -> String -> IO (Result AST)
parseFile Exp filepath = parseFromFileEx expressionWithApp filepath

parseString :: ParseType -> String -> Result AST
parseString Exp string = Parser.parseString expressionWithApp zeroDelta string

maybeAnnotateExpression annotatableExpressions = (do
  startSpan <- getPosition
  dynExp    <- annotatableExpressions
  maybeTy   <- optional annotation
  case maybeTy of
    Nothing -> return dynExp
    Just ty -> do
      endSpan <- getPosition
      return $ AAnnExp dynExp ty (makeSpan startSpan endSpan)
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

type_ = withSpan $ (do
  firstChar <- oneOf tyStartChars <?> "start of type"
  rest      <- (many $ oneOf tyChars) <?> "tail of type"
  whiteSpace
  let name = firstChar : rest
  return $ ATySym $ pack name) <?> "type"

tyStartChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
tyChars      = "abcdefghijklmnopqrstuvwxyz" ++ tyStartChars ++ digits
digits       = "0123456789"
