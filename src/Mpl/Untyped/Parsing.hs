module Mpl.Untyped.Parsing where

import           Mpl.Untyped.Syntax
import           Mpl.Common.Parsers
import           Mpl.ParserUtils
import           Mpl.Utils
import           Mpl.Prelude
import qualified Mpl.Common.Syntax  as CS

parseExpressionText :: Text -> ParseResult (SourceAnnotated (SyntaxF (SourceAnnotated CS.Binder)))
parseExpressionText = parseFromString untypedSyntaxConstructors commonParser . textToString

untypedSyntaxConstructors =
  SyntaxConstructors
    { consExpression       = id
    , consCommon           = Common
    , consBinder           = id
    }
