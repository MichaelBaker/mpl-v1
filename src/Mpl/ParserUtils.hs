-- | Datatype and functions that can be reused when implementing non-common lanugages.
--
-- These reusable components provide the following benefits:
--
--   * Faster modifications to both common and non-common lanugages via reduced code duplication.
--   * Explicitly encodes what capabilities parsers for non-common lanugages have available. e.g. which common syntactic elements can be altered.
--   * Ensures that all non-common languages will have uniform support from tooling because tooling can be implemented in terms of the abstractions present here.
module Mpl.ParserUtils
  ( module Mpl.ParserUtils
  , Text
  , Parser
  , ParserDescription(..)
  , Delta
  , (<|>)
  , many
  , some
  , oneOf
  , noneOf
  , whiteSpace
  , sepEndBy1
  , someSpace
  , try
  , optional
  , symbol
  , notFollowedBy
  , char
  , lookAhead
  , position
  , lift
  )
where

import Control.Applicative              ((<|>), many, some)
import Control.Monad.Trans.Class        (lift)
import Control.Monad.Trans.Reader       (ReaderT, runReaderT, mapReaderT, asks)
import Data.Functor.Foldable            (Base)
import Data.Semigroup                   (Semigroup, (<>))
import Data.Semigroup.Reducer           (Reducer, snoc)
import Data.Text                        (Text, pack, unpack)
import Mpl.Annotation                   (Annotated, Cofree((:<)), annotation)
import Mpl.Parser                       (Parser(..), Result, withDescription, parseByteString, getState, modifyingState)
import Mpl.ParserDescription            (ParserDescription(..))
import Mpl.Rendering
import Mpl.Utils                        (ByteString, textToString, stringToText, byteStringToString, stringToByteString, byteStringSlice, byteStringToText)
import Prelude                          (Bool(..), Show, Eq, Maybe(..), (==), Either(..), (++), (.), ($), (<*>), (*>), Integer, String, Monoid, const, drop, mappend, mempty, fromIntegral, return, mempty)
import Text.Parser.Char                 (noneOf, oneOf, char, anyChar)
import Text.Parser.Combinators          (Parsing, try, optional, notFollowedBy, sepEndBy1, between, eof)
import Text.Parser.LookAhead            (lookAhead)
import Text.Parser.Token                (TokenParsing, whiteSpace, symbolic, symbol, someSpace)
import Text.Trifecta.Combinators        (MarkParsing(release), spanned, position, line)
import Text.Trifecta.Delta              (Delta(Columns), column, rewind, columnByte, delta)
import Text.Trifecta.Rendering          (Span(..), Spanned((:~)))
import Text.Trifecta.Rope
import Text.Trifecta.Util.It

import qualified Mpl.Common.Syntax as CS
import qualified Data.ByteString   as BS
import qualified Data.Set          as Set

-- | Parsers wich have access to the parsing context and produce @f@s tagged with source code information.
type MplParser binder f =
  GenericContextualParser
    (SourceAnnotated binder)
    (SourceAnnotated (f (SourceAnnotated binder)))

-- | Parsers which have access to a parsing context capable of working with @f@s.
--
-- This is useful because it prevents the context from depending upon the type of annotation being applied to the tree, which means that the amount of work necessary to change that annotation is limited.
type GenericContextualParser binder f =
  ReaderT
    (ParsingContext binder f)
    StatefulParser
    f

-- | Parsers that can have source annotations added to their results automatically.
--
-- The parser has access to a parsing context which can work with @f@s which have already been annotated.
--
-- The underlying monad is a parser carrying ParserState
--
-- The result of the parser is an @f@ which hasn't been annotated yet because the function accepting one of these will annotate it.
type SourceAnnotatable binder f =
  ReaderT
    (ParsingContext (SourceAnnotated binder) (SourceAnnotated (f (SourceAnnotated binder))))
    StatefulParser
    (f (SourceAnnotated binder) (SourceAnnotated (f (SourceAnnotated binder))))

-- | The result of a parse annotated with source code information.
--
-- @f@ must be a Functor because Annotated is fixed point. This is useful because it means @f@ can be annotated with source information without any reference to that information in the datatype itself.
type SourceAnnotated f = Annotated f SourceSpan

-- | A ParseResult contains the original input in addition to the result of the parse.
--
-- This allows access to arbitrary amounts of context around the source of an error should one be encontered.
type ParseResult a = (ByteString, Result ParserState a)

-- | A Parser carrying ParserState
type StatefulParser = Parser ParserState

data ParserState = ParserState
  { descriptionStack :: [ParserDescription] -- ^ Knowing what path we took to get to the current syntactic element helps with generating useful error messages.
  } deriving (Show)

instance Semigroup ParserState where
  a <> b = a

instance Monoid ParserState where
  mappend = (<>)
  mempty  = ParserState { descriptionStack = [] }

data ParsingContext binder a =
  ParsingContext
    { syntaxConstructors :: SyntaxConstructors binder a
    , currentDescription :: Maybe ParserDescription
    }

data SyntaxConstructors binder f
  = SyntaxConstructors
  { consExpression :: GenericContextualParser binder f -> GenericContextualParser binder f
    -- ^ Allows a non-common language to perform parsing before and after a common expression.
  , consCommon :: CS.SyntaxF binder f -> Base f f
    -- ^ Lifts a common expression into the non-common language.
  , consBinder :: StatefulParser (SourceAnnotated CS.Binder) -> StatefulParser binder
  }

data SourceSpan =
  SourceSpan
    { startDelta :: Delta
    , startLine  :: ByteString
    , endDelta   :: Delta
    }
  deriving (Show, Eq)

parseFromString :: SyntaxConstructors binder a -> GenericContextualParser binder a -> String -> ParseResult a
parseFromString syntaxConstructors mplParser string = (byteString, result)
  where byteString     = stringToByteString string
        result         = parseByteString parser zeroDelta byteString mempty
        parser         = runReaderT mplParser parsingContext
        parsingContext =
          ParsingContext
            { syntaxConstructors = syntaxConstructors
            , currentDescription = Nothing
            }

annotate' :: String -> String -> [String] -> StatefulParser (f (SourceAnnotated f)) -> StatefulParser (SourceAnnotated f)
annotate' name expectation examples parser = do
  firstChar   <- lookAhead anyChar
  beforeDelta <- position
  let startDelta = beforeDelta <> delta firstChar
  let description =
        ParserDescription
          { parserName        = name
          , parserExamples    = examples
          , parserDelta       = startDelta
          , parserExpectation = expectation
          }
  startLine    <- line
  result       <- modifyingState (\s -> s { descriptionStack = description : descriptionStack s }) parser
  endDelta     <- position
  return (SourceSpan startDelta startLine endDelta :< result)

annotate :: String -> String -> [String] -> SourceAnnotatable binder f -> MplParser binder f
annotate name expectation examples parser = do
  firstChar   <- lookAhead anyChar
  beforeDelta <- position
  let startDelta = beforeDelta <> delta firstChar
  let description =
        ParserDescription
          { parserName        = name
          , parserExamples    = examples
          , parserDelta       = startDelta
          , parserExpectation = expectation
          }
  startLine    <- line
  result       <- pushingDescription description parser
  endDelta     <- position
  return (SourceSpan startDelta startLine endDelta :< result)

pushingDescription description = mapReaderT (modifyingState (\s -> s { descriptionStack = description : descriptionStack s }))

withExpectation name expectation = mapReaderT (withDescription (ParserDescription name [] mempty expectation))

parens :: TokenParsing m => m a -> m a
parens = between (symbolic '(') (char ')')

makeCommon common = do
  constructor <- asks (consCommon . syntaxConstructors)
  return (constructor common)

makeExpression flatExpression = do
  transform <- asks (consExpression . syntaxConstructors)
  transform flatExpression

makeBinder binderParser = do
  transform <- asks (consBinder . syntaxConstructors)
  lift $ transform binderParser

zeroDelta = Columns 0 0

symbolStartChars =
  ['<', '>', '?', '~', '!', '@', '$', '%', '^', '&', '*', '-', '_', '+', '\\', '/', '|'] ++
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

upcaseChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

symbolChars = symbolStartChars ++ digits

digits = "0123456789"

fileEnd :: (TokenParsing m, Parsing m) => m ()
fileEnd = eof <|> (someSpace *> eof)
