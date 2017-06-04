module Mpl.Untyped.Parsing (module Mpl.Untyped.Parsing, parser) where

import           Mpl.Common.Parsing hiding (Syntax, Binder, AnnotatedSyntax)
import           Mpl.ParserUtils
import           Mpl.Prelude
import           Mpl.Utils
import qualified Mpl.Common.Syntax  as CS
import qualified Mpl.Parser         as Parser
import qualified Mpl.Untyped.Syntax as Syntax

parseString :: String -> ParseResult AnnotatedSyntax
parseString code = (byteString, result)
  where byteString = stringToByteString code
        result = Parser.parseByteString parser zeroDelta byteString mempty

------------------------------------------------------
-- Untyped Type Aliases

type Syntax =
  Syntax.SyntaxF Binder

type Binder =
  SourceAnnotated CS.Binder

type AnnotatedSyntax =
  SourceAnnotated Syntax

------------------------------------------------------
-- Untyped Syntax Instances

instance MakeSymbol Syntax where
  makeSymbol = Syntax.symbol

instance MakeInteger Syntax where
  makeInteger = Syntax.int

instance MakeUTF8 Syntax where
  makeUTF8 = Syntax.utf8String

instance MakeFunction Syntax CS.Binder where
  makeFunction parameters body = Syntax.function parameters body

instance MakeApplication Syntax where
  makeApplication = Syntax.application

instance ParseExpression Syntax where
  parseExpression parser = parser

instance NonApplication Syntax where
  parseNonApplication = nonApplication
