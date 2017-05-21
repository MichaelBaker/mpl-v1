module Mpl.Typed.Parsing where

import           Mpl.Annotation
import           Mpl.Common.Parsing hiding (Syntax, Binder, AnnotatedSyntax)
import           Mpl.ParserUtils
import           Mpl.Prelude
import           Mpl.Utils
import qualified Mpl.Common.Syntax  as CS
import qualified Mpl.Parser         as Parser
import qualified Mpl.Typed.Syntax   as Syntax

type SourceType   = SourceAnnotated Syntax.Type
type SourceBinder = SourceAnnotated (Syntax.Binder SourceType)
type SourceSyntax = SourceAnnotated (Syntax.SyntaxF SourceType SourceBinder)

parseString :: String -> ParseResult AnnotatedSyntax
parseString code = (byteString, result)
  where byteString = stringToByteString code
        result = Parser.parseByteString parser zeroDelta byteString mempty

type Syntax =
  Syntax.SyntaxF SourceType SourceBinder

type Binder =
  SourceAnnotated (Syntax.Binder SourceType)

type AnnotatedSyntax =
  SourceAnnotated Syntax

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
    let binder = (span :< Syntax.CommonBinder (CS.Binder symbol))
    annotated binder (Syntax.annotatedBinder binder)

instance ParseExpression Syntax where
  parseExpression parser = do
    expression <- parser
    annotated expression (Syntax.typeAnnotation expression)

annotated withoutAnnotation withAnnotation = do
  annotation <- lookAhead (optional $ char ':')
  case annotation of
    Nothing ->
      return withoutAnnotation
    Just _ ->
      annotate
        "type annotation"
        "a type annotation"
        ["a: Integer", "123 : Integer"]
        (do
          char ':'
          whiteSpace
          annotation <- parseType
          return $ withAnnotation annotation)

parseType =
  annotate
    "type"
    "a type"
    ["Integer"]
    (symbolText upcaseChars symbolChars >>= (return . Syntax.typeSymbol))
