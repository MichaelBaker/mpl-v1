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
import Prelude                          (Bool(..), Show, Maybe(..), (==), Either(..), (++), (.), ($), (<*>), (*>), Integer, String, Monoid, const, drop, mappend, mempty, show, fromIntegral, return, mempty)
import Text.Parser.Char                 (noneOf, oneOf, char, anyChar)
import Text.Parser.Combinators          (Parsing, try, optional, notFollowedBy, sepEndBy1, between, eof)
import Text.Parser.LookAhead            (lookAhead)
import Text.Parser.Token                (TokenParsing, whiteSpace, symbolic, symbol, someSpace)
import Text.Trifecta.Combinators        (MarkParsing(release), spanned, position, line)
import Text.Trifecta.Delta              (Delta(Columns), column, rewind, columnByte, delta)
import Text.Trifecta.Rendering          (Span(..), Spanned((:~)))
import Text.Trifecta.Rope
import Text.Trifecta.Util.It

import qualified Data.ByteString as BS
import qualified Data.Set        as Set

type MplParser f        = MplGenericParser (Parsed f)
type MplGenericParser f = ReaderT (ParsingContext f) (Parser ParserState) f
type MplAnnotatable f   = ReaderT (ParsingContext (Parsed f)) (Parser ParserState) (f (Parsed f))
type Parsed f           = Annotated f SourceSpan
type Parsable f         = (Show (f (Parsed f)))
type ParseResult a      = (ByteString, Result ParserState a)

data ParserState = ParserState { descriptionStack :: [ParserDescription] } deriving (Show)

instance Semigroup ParserState where
  a <> b = a

instance Monoid ParserState where
  mappend = (<>)
  mempty  = ParserState { descriptionStack = [] }

data ParsingContext a =
  ParsingContext
    { syntaxConstructors :: SyntaxConstructors a
    , currentDescription :: Maybe ParserDescription
    }

data SyntaxConstructors f =
  SyntaxConstructors
    { consInt              :: Integer -> Base f f
    , consSymbol           :: Text -> Base f f
    , consFunction         :: [f] -> f -> Base f f
    , consApplication      :: f -> [f] -> Base f f
    , consExpression       :: MplGenericParser f -> MplGenericParser f
    }

data SourceSpan =
  SourceSpan
    { startDelta :: Delta
    , startLine  :: ByteString
    , endDelta   :: Delta
    }
  deriving (Show)

parseFromString :: SyntaxConstructors a -> MplGenericParser a -> String -> ParseResult a
parseFromString syntaxConstructors mplParser string = (byteString, result)
  where byteString     = stringToByteString string
        result         = parseByteString parser zeroDelta byteString mempty
        parser         = runReaderT mplParser parsingContext
        parsingContext =
          ParsingContext
            { syntaxConstructors = syntaxConstructors
            , currentDescription = Nothing
            }

annotate :: String -> String -> [String] -> MplAnnotatable f -> MplParser f
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

makeInt int = do
  constructor <- asks (consInt . syntaxConstructors)
  return (constructor int)

makeSymbol text = do
  constructor <- asks (consSymbol . syntaxConstructors)
  return (constructor text)

makeFunction parameters body = do
  constructor <- asks (consFunction . syntaxConstructors)
  return (constructor parameters body)

makeApplication function arguments = do
  constructor <- asks (consApplication . syntaxConstructors)
  return (constructor function arguments)

makeExpression flatExpression = do
  constructor <- asks (consExpression . syntaxConstructors)
  constructor flatExpression

zeroDelta = Columns 0 0

symbolStartChars =
  ['<', '>', '?', '~', '!', '@', '$', '%', '^', '&', '*', '-', '_', '+', '\\', '/', '|'] ++
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

upcaseChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

symbolChars = symbolStartChars ++ digits

digits = "0123456789"

fileEnd :: (TokenParsing m, Parsing m) => m ()
fileEnd = eof <|> (someSpace *> eof)
