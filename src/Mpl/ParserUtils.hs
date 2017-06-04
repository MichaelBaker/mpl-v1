-- | Datatype and functions that can be reused when implementing non-common lanugages.
--
-- These reusable components provide the following benefits:
--
--   * Faster modifications to both common and non-common lanugages via reduced code duplication.
--   * Explicitly encodes what capabilities parsers for non-common lanugages have available. e.g. which common syntactic elements can be altered.
--   * Ensures that all non-common languages will have uniform support from tooling because tooling can be implemented in terms of the abstractions present here.
module Mpl.ParserUtils
  ( module Mpl.ParserUtils
  , (<|>)
  , Delta
  , Parser
  , ParserDescription(..)
  , Text
  , char
  , lift
  , lookAhead
  , many
  , noneOf
  , notFollowedBy
  , oneOf
  , optional
  , position
  , sepEndBy
  , sepEndBy1
  , some
  , someSpace
  , symbol
  , try
  , whiteSpace
  )
where

import           Control.Applicative        ((<|>), many, some)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Reader (ReaderT, runReaderT, mapReaderT, asks)
import           Data.Functor.Foldable      (Base)
import           Data.Semigroup             (Semigroup, (<>))
import           Data.Semigroup.Reducer     (Reducer, snoc)
import           Data.Text                  (Text, pack, unpack)
import           Mpl.Annotation
import           Mpl.Parser
import           Mpl.Parser.SourceSpan
import           Mpl.ParserDescription      (ParserDescription(..))
import           Mpl.Prelude
import           Mpl.Rendering hiding       (between)
import           Prelude                    (Bool(..), Show, Eq, Maybe(..), (==), Either(..), (++), (.), ($), (<*>), (*>), Integer, String, Monoid, const, drop, mappend, mempty, fromIntegral, return, mempty)
import           Text.Parser.Char           (noneOf, oneOf, char, anyChar)
import           Text.Parser.Combinators    (Parsing, try, optional, notFollowedBy, sepEndBy, sepEndBy1, between, eof)
import           Text.Parser.LookAhead      (lookAhead)
import           Text.Parser.Token          (TokenParsing, whiteSpace, symbolic, symbol, someSpace)
import           Text.Trifecta.Combinators  (MarkParsing(release), spanned, position, line)
import           Text.Trifecta.Delta        (Delta(Columns), column, rewind, columnByte, delta)
import           Text.Trifecta.Rendering    (Span(..), Spanned((:~)))
import           Text.Trifecta.Rope
import           Text.Trifecta.Util.It
import qualified Mpl.Common.Syntax          as CS
import qualified Mpl.ParserError            as ParserError

-- | The result of a parse annotated with source code information.
--
-- @f@ must be a Functor because Annotated is fixed point. This is useful because it means @f@ can be annotated with source information without any reference to that information in the datatype itself.
type SourceAnnotated f =
  Annotated f SourceSpan

-- | The result of a parse annotated with source code information.
type SourceUnannotated f =
  Unannotated (SourceAnnotated f)

-- | A ParseResult contains the original input in addition to the result of the parse.
--
-- This allows access to arbitrary amounts of context around the source of an error should one be encontered.
type ParseResult a = (ByteString, Result ParserState a)

-- | A Parser carrying ParserState
type StatefulParser = Parser ParserState

type StatefulError = ParserError.Error ParserState

data ParserState = ParserState
  { descriptionStack :: [ParserDescription] -- ^ Knowing what path we took to get to the current syntactic element helps with generating useful error messages.
  } deriving (Show)

instance Semigroup ParserState where
  a <> b = a

instance Monoid ParserState where
  mappend = (<>)
  mempty  = ParserState { descriptionStack = [] }

annotate :: String -> String -> [String] -> StatefulParser (f (SourceAnnotated f)) -> StatefulParser (SourceAnnotated f)
annotate name expectation examples parser = do
  (span, result) <- withAnnotation name expectation examples parser
  return (span :< result)

withAnnotation :: String -> String -> [String] -> StatefulParser f -> StatefulParser (SourceSpan, f)
withAnnotation name expectation examples parser = do
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
  startLine <- line
  result    <- parser
    |> modifyingState (\s -> s { descriptionStack = description : descriptionStack s })
    |> modifyingUncommittedErrorState (\s -> s { descriptionStack = drop 1 (descriptionStack s)})
  endDelta  <- position
  return (SourceSpan startDelta startLine endDelta, result)

withExpectation name expectation =
  withDescription (ParserDescription name [] mempty expectation)

parens :: TokenParsing m => m a -> m a
parens = between (symbolic '(') (char ')')

zeroDelta = Columns 0 0

symbolStartChars =
  ['<', '>', '?', '~', '!', '@', '$', '%', '^', '&', '*', '-', '_', '+', '\\', '/', '|'] ++
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

upcaseChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

symbolChars = symbolStartChars ++ digits

digits = "0123456789"

fileEnd :: (TokenParsing m, Parsing m) => m ()
fileEnd = eof <|> (someSpace *> eof)
