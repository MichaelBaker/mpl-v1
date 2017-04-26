module Mpl.Common.Parsing where

import Mpl.Common.Parsers
import Mpl.Common.Syntax
import Mpl.ParserUtils
import Mpl.Prelude
import Mpl.Utils

parseExpressionText :: Text -> ParseResult (SourceAnnotated (SyntaxF (SourceAnnotated Binder)))
parseExpressionText = parseFromString commonSyntaxConstructors commonParser . textToString

commonSyntaxConstructors =
  SyntaxConstructors
    { consExpression       = id
    , consCommon           = id
    , consBinder           = id
    }
