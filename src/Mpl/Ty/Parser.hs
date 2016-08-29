module Mpl.Ty.Parser where

import Mpl.Ty.AST (AST(..), TyAST(..))

import Mpl.Span                  (Span(..))
import Control.Applicative       ((<|>), many, some)
import Data.Text                 (pack)
import Data.ByteString.Char8     (unpack)
import Text.Parser.Char          (oneOf)
import Text.Parser.Token         (whiteSpace, symbolic, parens)
import Text.Parser.Combinators   ((<?>), try, optional)
import Text.Trifecta.Delta       (Delta(Directed, Columns, Tab, Lines))
import Text.Trifecta.Result      (Result())
import Text.Trifecta.Parser      (parseFromFileEx)
import Text.Trifecta.Combinators (DeltaParsing(), position)

import qualified Mpl.Dyn.Parser as Dyn
import qualified Text.Trifecta.Parser as Parser

data ParseType = Exp -- | Prog | Def

parseFile :: ParseType -> String -> IO (Result AST)
parseFile Exp  filepath = parseFromFileEx expression filepath
-- parseFile Prog filepath = parseFromFileEx program           filepath
-- parseFile Def  filepath = parseFromFileEx definition        filepath

parseString :: ParseType -> String -> Result AST
parseString Exp  string = Parser.parseString expression Dyn.zeroDelta string
-- parseString Prog string = Parser.parseString program           zeroDelta string
-- parseString Def  string = Parser.parseString definition        zeroDelta string

expression = (do
  startSpan <- position
  dynExp    <- annotatableExpressions
  maybeTy   <- optional annotation
  case maybeTy of
    Nothing -> return dynExp
    Just ty -> do
      endSpan <- position
      return $ AAnnExp dynExp ty (makeSpan startSpan endSpan)
  ) <?> "expression"

annotation = symbolic ':' *> whiteSpace *> type_ <?> "type annotation"

annotatableExpressions =
  -- -- Parentheticals
  --     try lambda
      parens expression
  <|> Dyn.makeLetExp ALet definition expression
  <|> (ADyn <$> Dyn.utf16)
  <|> Dyn.makeLens ALens ADyn expression
  <|> (ADyn <$> try Dyn.real)
  <|> (ADyn <$> Dyn.int)
  <|> Dyn.record ARec (Dyn.recordField AField expression)
  <|> Dyn.list AList expression
  <|> (ADyn <$> Dyn.symbol)

definition = Dyn.makeDefinition ADef ALam binding
binding    = Dyn.makeBinding expression

type_ = withSpan $ (do
  firstChar <- oneOf tyStartChars <?> "start of type"
  rest      <- (many $ oneOf tyChars) <?> "tail of type"
  whiteSpace
  let name = firstChar : rest
  return $ ATySym $ pack name) <?> "type"

tyStartChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
tyChars      = "abcdefghijklmnopqrstuvwxyz" ++ tyStartChars ++ digits
digits       = "0123456789"

withSpan :: (DeltaParsing m) => m (Span -> a) -> m a
withSpan parser = do
  startSpan <- position
  item      <- parser
  endSpan   <- position
  return $ item (makeSpan startSpan endSpan)

makeSpan startSpan endSpan = Span (unpack filePath) startByte endByte
  where (filePath, startByte) = case startSpan of
                                  Columns _ bytes           -> ("<no file>", bytes)
                                  Tab _ _ bytes             -> ("<no file>", bytes)
                                  Lines _ _ bytes _         -> ("<no file>", bytes)
                                  Directed file _ _ bytes _ -> (file, bytes)
        endByte = case endSpan of
                    Columns _ bytes        -> bytes
                    Tab _ _ bytes          -> bytes
                    Lines _ _ bytes _      -> bytes
                    Directed _ _ _ bytes _ -> bytes
