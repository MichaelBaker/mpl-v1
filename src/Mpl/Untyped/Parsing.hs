module Mpl.Untyped.Parsing where

import Mpl.Untyped.Syntax
  ( SyntaxF (Common)
  , int
  , binder
  , symbol
  , function
  , application
  )

import Mpl.Common.Parsers (commonParser)

import Mpl.ParserUtils
  ( ParseResult
  , SyntaxConstructors(..)
  , SourceAnnotated
  , parseFromString
  )

import Mpl.Utils
  ( Text
  , textToString
  )

import qualified Mpl.Common.Syntax as CS

parseExpressionText :: Text -> ParseResult (SourceAnnotated (SyntaxF (SourceAnnotated CS.Binder)))
parseExpressionText = parseFromString syntaxConstructors commonParser . textToString

syntaxConstructors =
  SyntaxConstructors
    { consExpression       = id
    , consCommon           = Common
    , consBinder           = id
    }
