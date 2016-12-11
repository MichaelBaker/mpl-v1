module Mpl.ParsingUtils
  ( module Mpl.ParsingUtils
  , Text
  , Result(Success, Failure)
  , Parser
  , ParserDescription(..)
  , ParserSuggestion(..)
  , Err(..)
  , (<|>)
  , many
  , some
  , oneOf
  , whiteSpace
  , try
  , parens
  , symbolic
  , optional
  , symbol
  , notFollowedBy
  , char
  , lookAhead
  )
where

import Control.Applicative        ((<|>), many, some)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, mapReaderT, asks, local)
import Data.Functor.Foldable      (Base)
import Data.Semigroup.Reducer     (Reducer, snoc)
import Data.Text                  (Text, pack, unpack)
import Mpl.Annotation             (Annotated, Cofree((:<)))
import Mpl.ParserDescription      (ParserDescription(..), ParserSuggestion(..))
import Mpl.ParserResult           (Result(Success, Failure), Err(..))
import Mpl.Parser                 (Parser(..), mapError, parseString, addDescription)
import Mpl.Utils                  (textToString, stringToText)
import Prelude                    (Show, Maybe(..), (++), (.), ($), (<*>), (*>), Integer, String, return, mempty)
import Text.Parser.Char           (oneOf, char)
import Text.Parser.Combinators    (try, optional, notFollowedBy)
import Text.Parser.LookAhead      (lookAhead)
import Text.Parser.Token          (whiteSpace, parens, symbolic, symbol)
import Text.Trifecta.Combinators  (MarkParsing(release), spanned)
import Text.Trifecta.Delta        (Delta(Columns))
import Text.Trifecta.Rendering    (Span, Spanned((:~)))
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

setDescription description env = env { currentDescription = Just description }

infixr 5 !>
parser !> handle = do
  maybeDescription <- asks currentDescription
  case maybeDescription of
    Nothing ->
      parser
    Just description ->
      mapReaderT (`mapError` handle description) parser

parser !?> handle = do
  description <- asks currentDescription
  mapReaderT (`mapError` handle description) parser

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
