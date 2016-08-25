{-# LANGUAGE RecursiveDo #-}

module Mpl.Dyn.Parser where

import Prelude hiding (span)

import Mpl.Dyn.AST               (AST(..), Span(..), span)
import Control.Applicative       ((<|>), many, some)
import Data.Text                 (pack)
import Data.ByteString.Char8     (unpack)
import Text.Parser.Char          (oneOf)
import Text.Parser.Token         (TokenParsing(), integer, whiteSpace, someSpace, double, parens, brackets, braces, symbolic, stringLiteral, textSymbol)
import Text.Parser.Combinators   ((<?>), try, optional, sepEndBy, sepEndBy1, manyTill)
import Text.Trifecta.Delta       (Delta(Directed, Columns, Tab, Lines))
import Text.Trifecta.Result      (Result())
import Text.Trifecta.Parser      (parseFromFileEx)
import Text.Trifecta.Combinators (DeltaParsing(), position)

import qualified Text.Trifecta.Delta  as Delta
import qualified Text.Trifecta.Parser as Parser

data ParseType = Exp | Prog | Def

parseFile :: ParseType -> String -> IO (Result AST)
parseFile Exp  filepath = parseFromFileEx expressionWithApp filepath
parseFile Prog filepath = parseFromFileEx program           filepath
parseFile Def  filepath = parseFromFileEx definition        filepath

parseString :: ParseType -> String -> Result AST
parseString Exp  string = Parser.parseString expressionWithApp zeroDelta string
parseString Prog string = Parser.parseString program           zeroDelta string
parseString Def  string = Parser.parseString definition        zeroDelta string

program = withSpan $ AProg <$> recursiveDefinitions <?> "program"

expressionWithApp =
      try lensApplication
  <|> try application
  <|> expression

expression =
  -- Parentheticals
      try lambda
  <|> parens expressionWithApp

  -- Other
  <|> let_exp

  -- Literals
  <|> utf16
  <|> lens
  <|> try real
  <|> int
  <|> record
  <|> list
  <|> symbol
  <?> "expression"

application = withSpan $ AApp <$> expression <*> some expression <?> "application"

lensApplication = withSpan $ cons <$> expression <*> lens <?> "lens application"
  where cons exp lens = ALensApp lens exp

recursiveDefinitions = withSpan $ ARecDefs <$> many (definition <* whiteSpace) <?> "recursive definitions"

let_exp = withSpan $ ALet <$> (textSymbol "let" *> many (try definition) <* textSymbol "in") <*> expression <?> "let"

record = withSpan $ braces $ ARec <$> sepEndBy recordField (floating $ symbolic ',') <?> "record"

recordField = withSpan $ AField <$> fieldLabel <* whiteSpace <* symbolic ':' <* whiteSpace <*> expression <?> "record field"

fieldLabel = int <|> symbol <?> "field label"

list = withSpan $ brackets $ AList <$> sepEndBy expression (floating $ symbolic ',') <?> "list"

utf16 = withSpan $ AUtf16 <$> stringLiteral <?> "utf16"

lens = withSpan $ ALens <$> some lensPart <?> "lens"

lensPart = symbolic '.' *> (int <|> symbol <|> parens expression)

lambda = withSpan (parens $ do
  symbolic '#'
  whiteSpace
  argFun <- optional (try binding)
  case argFun of
    Just (args, body) -> return $ ALam args body
    Nothing -> do
      body <- expression
      return $ ALam [] body) <?> "lambda"

int = withSpan $ AInt <$> integer <?> "integer"

real = withSpan $ (do
  neg <- optional (symbolic '-')
  val <- double
  return $ case neg of
    Nothing -> AReal val
    Just _  -> AReal (-val)) <?> "real number"

definition = withSpan $ (do
  startSpan <- position
  sym <- symbol
  whiteSpace
  (args, body) <- binding
  endSpan <- position
  return $ if null args
    then ADef sym body
    else ADef sym (ALam args body $ makeSpan startSpan endSpan)) <?> "definition"

binding = do
  args <- manyTill symbol (symbolic '=')
  body <- expression
  return $ (args, body)

symbol :: (Monad m, TokenParsing m, DeltaParsing m) => m AST
symbol = withSpan $ (do
  firstChar <- oneOf symStartChars <?> "start of symbol"
  rest      <- (many $ oneOf symChars) <?> "tail of symbol"
  whiteSpace
  let name = firstChar : rest
  if name `elem` reservedSymbols
    then fail $ "Reserved symbol: " ++ show name
    else return $ ASym $ pack name) <?> "symbol"

reservedSymbols = ["let", "in"]

reservedChars = ['.']

symStartChars =
  ['<', '>', '?', '~', '!', '@', '#', '$', '%', '^', '&', '*', '-', '_', '+', '\\', '/', '|'] ++
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

symChars = symStartChars ++ digits

digits = "0123456789"

floating p = whiteSpace <* p <* whiteSpace

optionalTrailing2 a sep = do
  values   <- some $ try (a <* sep)
  trailing <- a <* optional sep
  return $ values ++ [trailing]

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

zeroDelta = Delta.Columns 0 0

