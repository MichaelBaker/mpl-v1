{-# LANGUAGE RecursiveDo #-}

module Mpl.Dyn.Parser where

import Mpl.Dyn.AST               (AST(..))
import Control.Applicative       ((<|>), many, some)
import Data.Text                 (pack)
import Text.Parser.Char          (oneOf)
import Text.Parser.Token         (TokenParsing(), integer, whiteSpace, someSpace, double, parens, brackets, braces, symbolic, stringLiteral, textSymbol)
import Text.Parser.Combinators   ((<?>), try, optional, sepEndBy, sepEndBy1, manyTill)

import qualified Mpl.Parse as Parse

data ParseType = Exp | Prog | Def

parseFile :: Parse.FileParser ParseType AST
parseFile Exp  filepath = Parse.parseFromFile expressionWithApp filepath
parseFile Prog filepath = Parse.parseFromFile program           filepath
parseFile Def  filepath = Parse.parseFromFile definition        filepath

parseString :: Parse.StringParser ParseType AST
parseString Exp  string = Parse.parseFromString expressionWithApp Parse.zeroDelta string
parseString Prog string = Parse.parseFromString program           Parse.zeroDelta string
parseString Def  string = Parse.parseFromString definition        Parse.zeroDelta string

program = Parse.withSpan $ AProg <$> recursiveDefinitions <?> "program"

expressionWithApp = makeExpressionWithApp AApp ALensApp expression lens

makeExpressionWithApp appCons lensCons expression lens =
      try (lensApplication lensCons expression lens)
  <|> try (application appCons expression)
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

application cons expression = Parse.withSpan $ cons <$> expression <*> some expression <?> "application"

lensApplication cons expression lens = Parse.withSpan $ flip cons <$> expression <*> lens <?> "lens application"

recursiveDefinitions = Parse.withSpan $ ARecDefs <$> many (definition <* whiteSpace) <?> "recursive definitions"

let_exp = makeLetExp ALet definition expression

makeLetExp cons definition expression =
      Parse.withSpan
   $  cons
  <$> (textSymbol "let" *> many (try definition) <* textSymbol "in")
  <*> expression
  <?> "let"

record cons fieldParser = Parse.withSpan $ braces $ cons <$> sepEndBy fieldParser (floating $ symbolic ',') <?> "record"

recordField cons subExpression = Parse.withSpan $ cons <$> fieldLabel <* whiteSpace <* symbolic ':' <* whiteSpace <*> subExpression <?> "record field"

fieldLabel = int <|> symbol <?> "field label"

list cons subExpression = Parse.withSpan $ brackets $ cons <$> sepEndBy subExpression (floating $ symbolic ',') <?> "list"

utf16 = Parse.withSpan $ AUtf16 <$> stringLiteral <?> "utf16"

lens = makeLens ALens id expression

makeLens cons partCons subExpression = Parse.withSpan $ cons <$> some (makeLensPart partCons subExpression) <?> "lens"

makeLensPart cons subExpression = symbolic '.' *> ((cons <$> int) <|> (cons <$> symbol) <|> parens subExpression)

lambda = makeLambda ALam binding expression

makeLambda cons binding expression = Parse.withSpan (parens $ do
  symbolic '#'
  whiteSpace
  argFun <- optional (try binding)
  case argFun of
    Just (args, body) -> return $ cons args body
    Nothing -> do
      body <- expression
      return $ cons [] body) <?> "lambda"

int = Parse.withSpan $ AInt <$> integer <?> "integer"

real = Parse.withSpan $ (do
  neg <- optional (symbolic '-')
  val <- double
  return $ case neg of
    Nothing -> AReal val
    Just _  -> AReal (-val)) <?> "real number"

definition = makeDefinition ADef ALam binding

makeDefinition cons lamCons binding = Parse.withSpan $ (do
  startSpan <- Parse.getPosition
  sym <- symbol
  whiteSpace
  (args, body) <- binding
  endSpan <- Parse.getPosition
  return $ if null args
    then cons sym body
    else cons sym (lamCons args body $ Parse.makeSpan startSpan endSpan)) <?> "definition"

binding = makeBinding expression

makeBinding expression = do
  args <- manyTill symbol (symbolic '=')
  body <- expression
  return $ (args, body)

symbol = Parse.withSpan $ (do
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
