{-# LANGUAGE RecursiveDo #-}

module Mpl.Dyn.Parser where

import Prelude hiding (span)

import Mpl.Span                  (Span(..))
import Mpl.Dyn.AST               (AST(..), span)
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
  <|> record ARec (recordField AField expression)
  <|> list AList expression
  <|> symbol
  <?> "expression"

application = withSpan $ AApp <$> expression <*> some expression <?> "application"

lensApplication = withSpan $ cons <$> expression <*> lens <?> "lens application"
  where cons exp lens = ALensApp lens exp

recursiveDefinitions = withSpan $ ARecDefs <$> many (definition <* whiteSpace) <?> "recursive definitions"

let_exp = makeLetExp ALet definition expression

makeLetExp cons definition expression =
      withSpan
   $  cons
  <$> (textSymbol "let" *> many (try definition) <* textSymbol "in")
  <*> expression
  <?> "let"

record cons fieldParser = withSpan $ braces $ cons <$> sepEndBy fieldParser (floating $ symbolic ',') <?> "record"

recordField cons subExpression = withSpan $ cons <$> fieldLabel <* whiteSpace <* symbolic ':' <* whiteSpace <*> subExpression <?> "record field"

fieldLabel = int <|> symbol <?> "field label"

list cons subExpression = withSpan $ brackets $ cons <$> sepEndBy subExpression (floating $ symbolic ',') <?> "list"

utf16 = withSpan $ AUtf16 <$> stringLiteral <?> "utf16"

lens = makeLens ALens id expression

makeLens cons partCons subExpression = withSpan $ cons <$> some (makeLensPart partCons subExpression) <?> "lens"

makeLensPart cons subExpression = symbolic '.' *> ((cons <$> int) <|> (cons <$> symbol) <|> parens subExpression)

lambda = makeLambda ALam binding expression

makeLambda cons binding expression = withSpan (parens $ do
  symbolic '#'
  whiteSpace
  argFun <- optional (try binding)
  case argFun of
    Just (args, body) -> return $ cons args body
    Nothing -> do
      body <- expression
      return $ cons [] body) <?> "lambda"

int = withSpan $ AInt <$> integer <?> "integer"

real = withSpan $ (do
  neg <- optional (symbolic '-')
  val <- double
  return $ case neg of
    Nothing -> AReal val
    Just _  -> AReal (-val)) <?> "real number"

definition = makeDefinition ADef ALam binding

makeDefinition cons lamCons binding = withSpan $ (do
  startSpan <- position
  sym <- symbol
  whiteSpace
  (args, body) <- binding
  endSpan <- position
  return $ if null args
    then cons sym body
    else cons sym (lamCons args body $ makeSpan startSpan endSpan)) <?> "definition"

binding = makeBinding expression

makeBinding expression = do
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

-- TODO: Pull this into a shared module
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

