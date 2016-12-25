module Mpl.ParserUtils
  ( module Mpl.ParserUtils
  , Text
  , Result(Success, Failure)
  , Parser
  , ParserDescription(..)
  , SyntaxError(..)
  , SpecificError(..)
  , Delta
  , (<|>)
  , many
  , some
  , oneOf
  , whiteSpace
  , sepEndBy1
  , someSpace
  , try
  , parens
  , optional
  , symbol
  , notFollowedBy
  , char
  , lookAhead
  , position
  , fileEnd
  )
where

import Control.Applicative        ((<|>), many, some)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, mapReaderT, asks, local)
import Control.Monad.Trans.Class  (lift)
import Data.Functor.Foldable      (Base)
import Data.Semigroup.Reducer     (Reducer, snoc)
import Data.Text                  (Text, pack, unpack)
import Mpl.Annotation             (Annotated, Cofree((:<)), annotation)
import Mpl.ParserDescription      (ParserDescription(..))
import Mpl.ParserResult           (Result(Success, Failure), SyntaxError(..), SpecificError(..), raiseErr)
import Mpl.Parser                 (Parser(..), captureError, parseString, addDescription, getSlice)
import Mpl.Utils                  (textToString, stringToText, byteStringToString, byteStringSlice, byteStringToText)
import Prelude                    (Show, Maybe(..), Either(..), (++), (.), ($), (<*>), (*>), Integer, String, show, fromIntegral, return, mempty)
import Text.Parser.Char           (oneOf, char)
import Text.Parser.Combinators    (Parsing, try, optional, notFollowedBy, sepEndBy1, between, eof)
import Text.Parser.LookAhead      (lookAhead)
import Text.Parser.Token          (TokenParsing, whiteSpace, symbolic, symbol, someSpace)
import Text.Trifecta.Combinators  (MarkParsing(release), spanned, position, line)
import Text.Trifecta.Delta        (Delta(Columns), column, rewind, columnByte)
import Text.Trifecta.Rendering    (Span(..), Spanned((:~)))
import Text.Trifecta.Rope
import Text.Trifecta.Util.It

import qualified Data.ByteString as BS
import qualified Data.Set        as Set

type MplParser f        = MplGenericParser (Parsed f)
type MplGenericParser f = ReaderT (ParsingContext f) Parser f
type MplAnnotatable f   = ReaderT (ParsingContext (Parsed f)) Parser (f (Parsed f))
type Parsed f           = Annotated f Span
type Parsable f         = (Show (f (Parsed f)))

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
    , consLeftAssociative  :: f -> Base f f
    , consRightAssociative :: f -> Base f f
    }

parseFromString :: SyntaxConstructors a -> MplGenericParser a -> String -> Result a
parseFromString syntaxConstructors mplParser = parseString parser zeroDelta
  where parser         = runReaderT mplParser parsingContext
        parsingContext =
          ParsingContext
            { syntaxConstructors = syntaxConstructors
            , currentDescription = Nothing
            }

annotate :: String -> [String] -> MplAnnotatable f -> MplParser f
annotate name examples parser = do
  let description =
        RichDescription
          { name     = name
          , examples = examples
          }
  local (setDescription description) $ do
    result <- spanned (mapReaderT (addDescription description) parser)
    case result of
      value :~ span -> return (span :< value)

originalByteString annotatedElement =
  case annotation annotatedElement of
    Span startDelta endDelta  byteString ->
      let startChar = fromIntegral $ column startDelta
          endChar   = fromIntegral $ column endDelta
      in byteStringSlice startChar endChar byteString

parens :: TokenParsing m => m a -> m a
parens = between (symbolic '(') (char ')')

originalString = byteStringToString . originalByteString

setDescription description env = env { currentDescription = Just description }

data ErrorParts =
  ErrorParts
    { outerPrefix :: Text
    , innerPrefix :: Text
    , innerSuffix :: Text
    , outerSuffix :: Text
    }

handleError startDelta handler parser = do
  maybeDescription <- asks currentDescription
  case maybeDescription of
    Nothing ->
      parser
    Just description -> do
      innerPosition <- position
      result        <- mapReaderT captureError parser
      case result of
        Right value -> return value
        Left  capturedError -> lift $ do
          let endDelta = errDelta capturedError
          currentLine <- line
          outerPrefix <- getSlice (rewind startDelta) startDelta
          innerPrefix <- getSlice startDelta innerPosition
          innerSuffix <- getSlice innerPosition endDelta
          let outerSuffix = BS.drop (fromIntegral $ columnByte endDelta) currentLine
          let errorParts = ErrorParts
                (byteStringToText outerPrefix)
                (byteStringToText innerPrefix)
                (byteStringToText innerSuffix)
                (byteStringToText outerSuffix)
          raiseErr (handler errorParts description capturedError)

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

makeLeftAssociative expression = do
  constructor <- asks (consLeftAssociative . syntaxConstructors)
  return (constructor expression)

makeRightAssociative expression = do
  constructor <- asks (consRightAssociative . syntaxConstructors)
  return (constructor expression)

zeroDelta = Columns 0 0

symbolStartChars =
  ['<', '>', '?', '~', '!', '@', '#', '$', '%', '^', '&', '*', '-', '_', '+', '\\', '/', '|'] ++
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

upcaseChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

symbolChars = symbolStartChars ++ digits

digits = "0123456789"

fileEnd :: (TokenParsing m, Parsing m) => m ()
fileEnd = eof <|> (someSpace *> eof)
