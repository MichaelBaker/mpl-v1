module Mpl.Common.Parsing where

import           Mpl.Parser.SourceSpan
import           Mpl.ParserUtils       hiding (symbol, makeBinder)
import           Mpl.Prelude
import           Mpl.Utils
import qualified Mpl.Common.Syntax     as Syntax
import qualified Mpl.Parser            as Parser
import qualified Text.Parser.Token     as Token

------------------------------------------------------
-- Parsing Type Classes

class MakeSymbol syntax where
  makeSymbol :: Text -> SourceUnannotated syntax

class MakeInteger syntax where
  makeInteger :: Integer -> SourceUnannotated syntax

class MakeUTF8 syntax where
  makeUTF8 :: Text -> SourceUnannotated syntax

class MakeFunction syntax binder | syntax -> binder where
  makeFunction :: [binder] -> SourceAnnotated syntax -> SourceUnannotated syntax

class MakeApplication syntax where
  makeApplication :: SourceAnnotated syntax -> [SourceAnnotated syntax] -> SourceUnannotated syntax

class ParseBinder binder where
  parseBinder :: StatefulParser (SourceSpan, Text) -> StatefulParser binder

class ParseExpression syntax where
  parseExpression :: StatefulParser (SourceAnnotated syntax) -> StatefulParser (SourceAnnotated syntax)

------------------------------------------------------
-- Parsers

type SyntaxParser syntax binder =
  ( MakeSymbol      syntax
  , MakeInteger     syntax
  , MakeUTF8        syntax
  , MakeApplication syntax
  , MakeFunction    syntax binder
  , ParseExpression syntax
  , ParseBinder     binder
  )

parser :: (SyntaxParser syntax binder) => StatefulParser (SourceAnnotated syntax)
parser = applicationOrExpression <* fileEnd

expression :: (SyntaxParser syntax binder) => StatefulParser (SourceAnnotated syntax)
expression = flatExpression <|> (parens applicationOrExpression)

applicationOrExpression :: (SyntaxParser syntax binder) => StatefulParser (SourceAnnotated syntax)
applicationOrExpression = application <|> expression

flatExpression :: (SyntaxParser syntax binder) => StatefulParser (SourceAnnotated syntax)
flatExpression = parseExpression (int <|> utf8String <|> function <|> symbol)

application :: (SyntaxParser syntax binder) => StatefulParser (SourceAnnotated syntax)
application = parseExpression $
  annotate
    "function application"
    "a function application"
    ["f 1", "#(a = a + 1) 1"]
    (do
      function <- try $ do
        function <- expression
        someSpace
        notFollowedBy someSpace
        return function
      arguments <- sepEndBy1 expression someSpace
      return $ makeApplication function arguments)

function :: (SyntaxParser syntax binder) => StatefulParser (SourceAnnotated syntax)
function =
  annotate
    "anonymous function"
    "an anonymous function"
    ["#(a = a + 1)", "#(a b f = f a b 3)"]
    (do
      char '#'
      char '('
      whiteSpace
      parameters <- sepEndBy1 (parseBinder binder) someSpace
      char '='
      whiteSpace
      body <- withExpectation "expression" "an expression" applicationOrExpression
      whiteSpace
      char ')'
      return $ makeFunction parameters body)

int :: (MakeInteger syntax) => StatefulParser (SourceAnnotated syntax)
int =
  annotate
    "integer"
    "an integer"
    ["123", "0123", "-00000123"]
    (do
      isMinus <- optional $ char '-'
      int     <- some (oneOf digits)
      return $ case isMinus of
        Nothing -> makeInteger $ read int
        Just _  -> makeInteger $ negate $ read int)

utf8String :: (MakeUTF8 syntax) => StatefulParser (SourceAnnotated syntax)
utf8String =
  annotate
    "UTF8 string"
    "a UTF8 string"
    ["\"this is a string\"", "\"hexadecimal \\x12af\"", "\"octal \\o1274\""]
    (do
      text <- Token.stringLiteral
      return $ makeUTF8 text)

symbol :: (MakeSymbol syntax) => StatefulParser (SourceAnnotated syntax)
symbol =
  annotate
    "symbol"
    "a symbol"
    ["a", "<?>", "Hello", "a0~"]
    (do
      text <- symbolText symbolStartChars symbolChars
      return $ makeSymbol text)

binder :: StatefulParser (SourceSpan, Text)
binder =
  withAnnotation
    "binder"
    "a binder"
    ["a", "<?>", "Hello", "a0~"]
    (do
      text <- symbolText symbolStartChars symbolChars
      return text)

symbolText firstChars restChars = do
  firstChar <- oneOf firstChars
  rest      <- many (oneOf restChars)
  return $ stringToText (firstChar : rest)

------------------------------------------------------
-- Common Syntax Instances

type Syntax =
  Syntax.SyntaxF Binder

type AnnotatedSyntax =
  SourceAnnotated (Syntax.SyntaxF Binder)

type Binder =
  SourceAnnotated Syntax.Binder

instance MakeSymbol Syntax where
  makeSymbol = Syntax.symbol

instance MakeInteger Syntax where
  makeInteger = Syntax.int

instance MakeUTF8 Syntax where
  makeUTF8 = Syntax.utf8String

instance MakeFunction Syntax Binder where
  makeFunction parameters body = Syntax.function parameters body

instance MakeApplication Syntax where
  makeApplication = Syntax.application

instance ParseBinder Binder where
  parseBinder parser = do
    (span, symbol) <- parser
    return (span :< Syntax.binder symbol)

instance ParseExpression Syntax where
  parseExpression parser = parser

------------------------------------------------------
-- Utilities

parseString :: String -> ParseResult AnnotatedSyntax
parseString code = (byteString, result)
  where byteString = stringToByteString code
        result = Parser.parseByteString parser zeroDelta byteString mempty
