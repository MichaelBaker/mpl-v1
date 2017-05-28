module Mpl.Typed.Parsing where

import           Mpl.Annotation
import           Mpl.Common.Parsing hiding (Syntax, Binder, AnnotatedSyntax)
import           Mpl.ParserUtils
import           Mpl.Prelude
import           Mpl.Rendering
import           Mpl.Utils
import qualified Mpl.Common.Syntax            as CS
import qualified Mpl.Parser                   as Parser
import qualified Mpl.Typed.Syntax             as Syntax
import qualified Text.PrettyPrint.ANSI.Leijen as P

parseString :: String -> ParseResult AnnotatedSyntax
parseString code = (byteString, result)
  where byteString = stringToByteString code
        result = Parser.parseByteString parser zeroDelta byteString mempty

------------------------------------------------------
-- Typed Type Aliases

type Syntax =
  Syntax.SyntaxF SourceType SourceBinder

type Binder =
  SourceAnnotated (Syntax.Binder SourceType)

type AnnotatedSyntax =
  SourceAnnotated Syntax

type SourceType =
  SourceAnnotated Syntax.Type

type SourceBinder =
  SourceAnnotated (Syntax.Binder SourceType)

type SourceSyntax =
  SourceAnnotated (Syntax.SyntaxF SourceType SourceBinder)

------------------------------------------------------
-- Typed Syntax Instances

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

instance NonApplication Syntax where
  parseNonApplication = nonApplication

------------------------------------------------------
-- Typed Parsers

annotated withoutAnnotation withAnnotation = do
  whiteSpace
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

------------------------------------------------------
-- Typed Annotation Syntax Instances

parseType :: StatefulParser (SourceAnnotated Syntax.Type)
parseType = applicationOrExpression

instance MakeApplication Syntax.Type where
  makeApplication = Syntax.typeApplication

  makeApplicationAnnotation =
    annotate
      "type application"
      "a type application"
      ["-> Integer Integer"]

instance NonApplication Syntax.Type where
  parseNonApplication = parseTypeSymbol

instance MakeSymbol Syntax.Type where
  makeSymbol = Syntax.typeSymbol

instance ParseExpression Syntax.Type where
  parseExpression  = id

parseTypeSymbol =
  annotate
    "type symbol"
    "a type symbol"
    ["Integer"]
    (do
      text <- symbolText upcaseChars symbolChars
      return $ makeSymbol text)

------------------------------------------------------
-- Typed Pretty Printing

instance (Pretty type_, Pretty binder, Pretty recurse) => Pretty (Syntax.SyntaxF type_ binder recurse) where
  pretty (Syntax.Common common) =
    pretty common

  pretty (Syntax.TypeAnnotation expression annotation) =
    pretty expression <~> ": " <~> pretty annotation

instance (Pretty type_, Pretty recurse) => Pretty (Syntax.Binder type_ recurse) where
  pretty (Syntax.CommonBinder binder) =
    pretty binder

  pretty (Syntax.AnnotatedBinder binder annotation) =
    pretty binder <~> ": " <~> pretty annotation

instance (Pretty recurse) => Pretty (Syntax.Type recurse) where
  pretty (Syntax.TypeSymbol symbol) =
    pretty symbol

  pretty (Syntax.TypeApplication function arguments) =
    P.encloseSep "(" ")" " " (pretty function : fmap pretty arguments)
