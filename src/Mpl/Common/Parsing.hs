module Mpl.Common.Parsing where

import           Mpl.Parser.SourceSpan
import           Mpl.ParserUtils              hiding (symbol, makeBinder)
import           Mpl.Prelude
import           Mpl.Rendering
import           Mpl.Utils
import qualified Data.List                    as List
import qualified Mpl.Common.Syntax            as Syntax
import qualified Mpl.Parser                   as Parser
import qualified Text.Parser.Token            as Token
import qualified Text.PrettyPrint.ANSI.Leijen as P

------------------------------------------------------
-- Parsing Type Classes

class MakeApplication syntax where
  makeApplication :: SourceAnnotated syntax -> [SourceAnnotated syntax] -> SourceUnannotated syntax

class NonApplication syntax where
  parseNonApplication :: StatefulParser (SourceAnnotated syntax)

class ParseExpression syntax where
  parseExpression :: StatefulParser (SourceAnnotated syntax) -> StatefulParser (SourceAnnotated syntax)

class MakeSymbol syntax where
  makeSymbol :: Text -> SourceUnannotated syntax

class MakeInteger syntax where
  makeInteger :: Integer -> SourceUnannotated syntax

class MakeUTF8 syntax where
  makeUTF8 :: Text -> SourceUnannotated syntax

class MakeFunction syntax binder | syntax -> binder where
  makeFunction :: [SourceAnnotated binder] -> SourceAnnotated syntax -> SourceUnannotated syntax

class ParseBinder binder where
  makeBinder  :: Text -> SourceUnannotated binder
  parseBinder :: StatefulParser (SourceAnnotated binder) -> StatefulParser (SourceAnnotated binder)

------------------------------------------------------
-- | Application Parsers
--
-- This is a limited set of typeclasses necessary to implement nested application parsing. Languages can use this typeclass to reuse the nested language structure without supporting all of the syntactic elements of the full syntax parser.

type ApplicationSyntaxParser syntax =
  ( MakeApplication syntax
  , NonApplication  syntax
  , ParseExpression syntax
  )

parser :: (ApplicationSyntaxParser syntax) => StatefulParser (SourceAnnotated syntax)
parser = applicationOrExpression <* fileEnd

expression :: (ApplicationSyntaxParser syntax) => StatefulParser (SourceAnnotated syntax)
expression =
      parseExpression (parens applicationOrExpression)
  <|> applicationOrExpression

argument :: (ApplicationSyntaxParser syntax) => StatefulParser (SourceAnnotated syntax)
argument =
      parens applicationOrExpression
  <|> parseNonApplication

applicationOrExpression :: (ApplicationSyntaxParser syntax) => StatefulParser (SourceAnnotated syntax)
applicationOrExpression = parseExpression $ do
  (function:arguments) <- sepEndBy1 argument someSpace
  if List.null arguments
    then
      return function
    else do
      let newSpan = spanUnion (annotation function) (annotation $ List.last arguments)
      return (newSpan :< makeApplication function arguments)

------------------------------------------------------
-- | Syntax Parsers
--
-- This represents a parser which supports every syntactic element of the common syntax.

type SyntaxParser syntax binder =
  ( ApplicationSyntaxParser syntax
  , MakeFunction            syntax binder
  , MakeInteger             syntax
  , MakeSymbol              syntax
  , MakeUTF8                syntax
  , ParseBinder             binder
  )

nonApplication :: (SyntaxParser syntax binder) => StatefulParser (SourceAnnotated syntax)
nonApplication =
      int
  <|> utf8String
  <|> function
  <|> symbol

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
      parameters <- sepEndBy1 groupedBinder someSpace
      char '='
      whiteSpace
      body <- withExpectation "expression" "an expression" applicationOrExpression
      whiteSpace
      char ')'
      return $ makeFunction parameters body)

groupedBinder :: (ParseBinder binder) => StatefulParser (SourceAnnotated binder)
groupedBinder = parseBinder $
      parens groupedBinder
  <|> binder

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

binder :: (ParseBinder binder) => StatefulParser (SourceAnnotated binder)
binder =
  annotate
    "binder"
    "a binder"
    ["a", "<?>", "Hello", "a0~"]
    (do
      text <- symbolText symbolStartChars symbolChars
      return $ makeBinder text)

symbolText firstChars restChars = do
  firstChar <- oneOf firstChars
  rest      <- many (oneOf restChars)
  return $ stringToText (firstChar : rest)

------------------------------------------------------
-- Common Type Aliases

type Syntax =
  Syntax.SyntaxF Binder

type AnnotatedSyntax =
  SourceAnnotated (Syntax.SyntaxF Binder)

type Binder =
  SourceAnnotated Syntax.Binder

------------------------------------------------------
-- Common Syntax Instances

instance MakeSymbol Syntax where
  makeSymbol = Syntax.symbol

instance MakeInteger Syntax where
  makeInteger = Syntax.int

instance MakeUTF8 Syntax where
  makeUTF8 = Syntax.utf8String

instance MakeFunction Syntax Syntax.Binder where
  makeFunction parameters body = Syntax.function parameters body

instance MakeApplication Syntax where
  makeApplication = Syntax.application

instance ParseBinder Syntax.Binder where
  makeBinder =
    Syntax.binder

  parseBinder = id

instance ParseExpression Syntax where
  parseExpression parser = parser

instance NonApplication Syntax where
  parseNonApplication = nonApplication

------------------------------------------------------
-- Utilities

parseString :: String -> ParseResult AnnotatedSyntax
parseString code = (byteString, result)
  where byteString = stringToByteString code
        result = Parser.parseByteString parser zeroDelta byteString mempty

------------------------------------------------------
-- Common Pretty Printing

instance (Pretty recurse) => Pretty (Syntax.Binder recurse) where
  pretty (Syntax.Binder text) = pretty text

instance (Pretty binder, Pretty recurse) => Pretty (Syntax.SyntaxF binder recurse) where
  pretty (Syntax.Literal l)=
    case l of
      Syntax.IntegerLiteral i ->
        pretty i
      Syntax.UTF8StringLiteral s ->
        pretty s

  pretty (Syntax.Symbol symbol) =
    pretty symbol

  pretty (Syntax.Function parameter body) =
    P.encloseSep "#(" ")" " " [pretty parameter, "=", pretty body]

  pretty (Syntax.Application function argument) =
    P.encloseSep "(" ")" " " [pretty function, pretty argument]
