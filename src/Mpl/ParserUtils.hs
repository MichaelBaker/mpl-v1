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
  , noneOf
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

import Control.Applicative              ((<|>), many, some)
import Control.Monad.Trans.Class        (lift)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, gets, get, modify')
import Data.Functor.Foldable            (Base)
import Data.Semigroup.Reducer           (Reducer, snoc)
import Data.Text                        (Text, pack, unpack)
import Mpl.Annotation                   (Annotated, Cofree((:<)), annotation)
import Mpl.Parser                       (Parser(..), captureError, captureCommittedError, raiseCommitedError, parseString, addDescription, getSlice)
import Mpl.ParserDescription            (ParserDescription(..))
import Mpl.ParserResult                 (Result(Success, Failure), SyntaxError(..), SpecificError(..), raiseErr)
import Mpl.Rendering
import Mpl.Utils                        (ByteString, textToString, stringToText, byteStringToString, byteStringSlice, byteStringToText)
import Prelude                          (Show, Maybe(..), Either(..), (++), (.), ($), (<*>), (*>), Integer, String, show, fromIntegral, return, mempty)
import Text.Parser.Char                 (noneOf, oneOf, char)
import Text.Parser.Combinators          (Parsing, try, optional, notFollowedBy, sepEndBy1, between, eof)
import Text.Parser.LookAhead            (lookAhead)
import Text.Parser.Token                (TokenParsing, whiteSpace, symbolic, symbol, someSpace)
import Text.Trifecta.Combinators        (MarkParsing(release), spanned, position, line)
import Text.Trifecta.Delta              (Delta(Columns), column, rewind, columnByte)
import Text.Trifecta.Rendering          (Span(..), Spanned((:~)))
import Text.Trifecta.Rope
import Text.Trifecta.Util.It

import qualified Data.ByteString as BS
import qualified Data.Set        as Set

type MplParser f        = MplGenericParser (Parsed f)
type MplGenericParser f = StateT (ParsingContext f) Parser f
type MplAnnotatable f   = StateT (ParsingContext (Parsed f)) Parser (f (Parsed f))
type Parsed f           = Annotated f SourceSpan
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

data SourceSpan =
  SourceSpan
    { startDelta :: Delta
    , startLine  :: ByteString
    , endDelta   :: Delta
    }
  deriving (Show)

parseFromString :: SyntaxConstructors a -> MplGenericParser a -> String -> Result a
parseFromString syntaxConstructors mplParser = parseString parser zeroDelta
  where parser         = evalStateT mplParser parsingContext
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
  oldDescription <- gets currentDescription
  modify' (setDescription description)

  startDelta   <- position
  startLine    <- line
  currentState <- get
  let newParser = evalStateT parser currentState
  result       <- lift $ addDescription description $ captureCommittedError newParser
  endDelta     <- position

  result <-
    case result of
      Right value -> do
          return (SourceSpan startDelta startLine endDelta :< value)
      Left error ->
        lift $ case errSpecific error of
          SuggestionError {}        -> raiseCommitedError error
          ParserDescriptionError {} -> raiseCommitedError error
          _ -> do
            let endDelta = errDelta error
            currentLine <- line
            lineStart   <- getSlice (rewind startDelta) startDelta
            errorPart   <- getSlice startDelta endDelta
            let restOfLine = BS.drop (fromIntegral $ columnByte endDelta) currentLine
            let code = toDoc lineStart <~> problem errorPart <~> toDoc restOfLine
            raiseCommitedError $ error { errSpecific = ParserDescriptionError code description}

  modify' (\c -> c { currentDescription = oldDescription })
  return result

originalByteString annotatedElement =
  case annotation annotatedElement of
    SourceSpan startDelta byteString endDelta ->
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
  maybeDescription <- gets currentDescription
  case maybeDescription of
    Nothing ->
      parser
    Just description -> do
      innerPosition <- position
      currentState  <- get
      let newParser = evalStateT parser currentState
      result <- lift $ captureError newParser
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
  constructor <- gets (consInt . syntaxConstructors)
  return (constructor int)

makeSymbol text = do
  constructor <- gets (consSymbol . syntaxConstructors)
  return (constructor text)

makeFunction parameters body = do
  constructor <- gets (consFunction . syntaxConstructors)
  return (constructor parameters body)

makeApplication function arguments = do
  constructor <- gets (consApplication . syntaxConstructors)
  return (constructor function arguments)

makeExpression flatExpression = do
  constructor <- gets (consExpression . syntaxConstructors)
  constructor flatExpression

makeLeftAssociative expression = do
  constructor <- gets (consLeftAssociative . syntaxConstructors)
  return (constructor expression)

makeRightAssociative expression = do
  constructor <- gets (consRightAssociative . syntaxConstructors)
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
