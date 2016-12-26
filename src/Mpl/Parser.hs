{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}

module Mpl.Parser
  ( Parser(..)
  , Result
  , manyAccum
  , Step(..)
  , feed
  , starve
  , stepParser
  , stepResult
  , parseFromFile
  , parseFromFileEx
  , parseString
  , parseByteString
  , parseTest
  , modifyingState
  , getState
  , withDescription
  ) where

import Control.Applicative as Alternative
import Control.Monad (MonadPlus(..), ap, join)
import Control.Monad.IO.Class
import Data.ByteString as Strict hiding (empty, snoc)
import Data.ByteString.UTF8 as UTF8
import Data.Maybe (isJust)
import Data.Semigroup
import Data.Semigroup.Reducer
import Data.Set as Set hiding (empty, toList)
import System.IO
import Text.Parser.Combinators
import Text.Parser.Char
import Text.Parser.LookAhead
import Text.Parser.Token
import Text.PrettyPrint.ANSI.Leijen as Pretty hiding (line, (<>), (<$>), empty)
import Text.Trifecta.Combinators
import Text.Trifecta.Rendering
import Text.Trifecta.Rope
import Text.Trifecta.Delta as Delta
import Text.Trifecta.Util.It
import Mpl.ParserError
import Mpl.ParserDescription

-- | The type of a trifecta parser
--
-- The first four arguments are behavior continuations:
--
--   * epsilon success: the parser has consumed no input and has a result
--     as well as a possible Error; the position and chunk are unchanged
--     (see `pure`)
--
--   * epsilon failure: the parser has consumed no input and is failing
--     with the given Error; the position and chunk are unchanged (see
--     `empty`)
--
--   * committed success: the parser has consumed input and is yielding
--     the result, set of expected strings that would have permitted this
--     parse to continue, new position, and residual chunk to the
--     continuation.
--
--   * committed failure: the parser has consumed input and is failing with
--     a given Error (user-facing error message)
--
-- The remaining two arguments are
--
--   * the current position
--
--   * the chunk of input currently under analysis
--
-- `Parser` is an `Alternative`; trifecta's backtracking behavior encoded as
-- `<|>` is to behave as the leftmost parser which yields a value
-- (regardless of any input being consumed) or which consumes input and
-- fails.  That is, a choice of parsers will only yield an epsilon failure
-- if *all* parsers in the choice do.  If that is not the desired behavior,
-- see `try`, which turns a committed parser failure into an epsilon failure
-- (at the cost of error information).
--
newtype Parser s a = Parser
  { unparser :: forall r.
    (a -> Error s -> It Rope r) ->
    (Error s -> It Rope r) ->
    (a -> Set ParserDescription -> s -> Delta -> ByteString -> It Rope r) -> -- committed success
    (Error s -> It Rope r) ->                                                -- committed err
    s ->                                                                     -- custom state
    Delta -> ByteString -> It Rope r
  }

instance Functor (Parser state) where
  fmap f (Parser m) = Parser $ \ eo ee co -> m (eo . f) ee (co . f)
  {-# INLINE fmap #-}
  a <$ Parser m = Parser $ \ eo ee co -> m (\_ -> eo a) ee (\_ -> co a)
  {-# INLINE (<$) #-}

instance (Monoid state) => Applicative (Parser state) where
  pure a = Parser $ \ eo _ _ _ _ _ _ -> eo a mempty
  {-# INLINE pure #-}
  (<*>) = ap
  {-# INLINE (<*>) #-}

instance (Monoid state) => Alternative (Parser state) where
  empty = Parser $ \_ ee _ _ _ _ _ -> ee mempty
  {-# INLINE empty #-}
  Parser m <|> Parser n = Parser $ \ eo ee co ce s d bs ->
    m
      eo
      (\e ->
        n
          (\a e' -> eo a (e <> e'))
          (\e' -> ee (e <> e'))
          co ce s d bs)
      co ce s d bs
  {-# INLINE (<|>) #-}
  many p = Prelude.reverse <$> manyAccum (:) p
  {-# INLINE many #-}
  some p = (:) <$> p <*> Alternative.many p

instance (Monoid state, Semigroup a) => Semigroup (Parser state a) where
  (<>) = liftA2 (<>)
  {-# INLINE (<>) #-}

instance (Monoid state, Monoid a) => Monoid (Parser state a) where
  mappend = liftA2 mappend
  {-# INLINE mappend #-}
  mempty = pure mempty
  {-# INLINE mempty #-}

instance (Monoid state) => Monad (Parser state) where
  return a = Parser $ \ eo _ _ _ _ _ _ -> eo a mempty
  {-# INLINE return #-}
  Parser m >>= k = Parser $ \ eo ee co ce s d bs ->
    m -- epsilon result: feed result to monadic continutaion; committed
      -- continuations as they were given to us; epsilon callbacks merge
      -- error information with `<>`
      (\a e ->
        unparser (k a)
          (\b e' -> eo b (e <> e'))
          (\e'   -> ee (e <> e'))
          co ce s d bs)
      -- epsilon error: as given
      ee
      -- committed result: feed result to monadic continuation and...
      (\a es s' d' bs' -> unparser (k a)
         -- epsilon results are now committed results due to m consuming.
         --
         -- epsilon success is now committed success at the new position
         -- (after m), yielding the result from (k a) and merging the
         -- expected sets (i.e. things that could have resulted in a longer
         -- parse)
         (\b e' -> co b (es <> errorExpected e') s' d' bs')
         -- epsilon failure is now a committed failure at the new position
         -- (after m); compute the error to display to the user
         (\e -> ce $ e { errorType = CommittedError })
         -- committed behaviors as given; nothing exciting here
         co ce
         -- new position and remaining chunk after m
         s' d' bs')
      -- committed error, delta, and bytestring: as given
      ce s d bs
  {-# INLINE (>>=) #-}
  (>>) = (*>)
  {-# INLINE (>>) #-}
  fail s = Parser $ \ _ ee _ _ state d _ -> ee $ failed state d s EpsilonError
  {-# INLINE fail #-}

instance (Monoid state) => MonadPlus (Parser state) where
  mzero = empty
  {-# INLINE mzero #-}
  mplus = (<|>)
  {-# INLINE mplus #-}

failed state delta message errorType = Error state delta (Message message) mempty errorType False
{-# INLINE failed #-}

manyAccum :: (a -> [a] -> [a]) -> Parser state a -> Parser state [a]
manyAccum f (Parser p) = Parser $ \eo _ co ce s d bs ->
  let walk xs x es s' d' bs' = p (manyErr s' d' bs') (\e -> co (f x xs) (errorExpected e <> es) s' d' bs') (walk (f x xs)) ce s' d' bs'
      manyErr s' d' bs' _ e  = ce (e <> failed s' d' "'many' applied to a parser that accepted an empty string" CommittedError)
  in p (manyErr s d bs) (eo []) (walk []) ce s d bs

liftIt :: (Monoid state) => It Rope a -> Parser state a
liftIt m = Parser $ \ eo _ _ _ _ _ _ -> do
  a <- m
  eo a mempty
{-# INLINE liftIt #-}

instance (Monoid state) => Parsing (Parser state) where
  try (Parser m) = Parser $ \ eo ee co _ -> m eo ee co (\_ -> ee mempty)
  {-# INLINE try #-}
  Parser m <?> nm = Parser $ \ eo ee co ce s d bs ->
    let desc =
          ParserDescription
            { parserName        = nm
            , parserDelta       = d
            , parserExamples    = []
            , parserExpectation = nm
            }
    in m
         (\a e -> eo a (e { errorExpected = Set.singleton desc }))
         (\e   -> ee e { errorExpected    = Set.singleton desc })
         co ce s d bs
  {-# INLINE (<?>) #-}
  skipMany p = () <$ manyAccum (\_ _ -> []) p
  {-# INLINE skipMany #-}
  unexpected s = Parser $ \ _ ee _ _ state delta _ -> ee $ failed state delta ("unexpected " ++ s) EpsilonError
  {-# INLINE unexpected #-}
  eof = notFollowedBy anyChar <?> "end of input"
  {-# INLINE eof #-}
  notFollowedBy p = try (optional p >>= maybe (pure ()) (unexpected . show))
  {-# INLINE notFollowedBy #-}

instance (Monoid state) => LookAheadParsing (Parser state) where
  lookAhead (Parser m) = Parser $ \eo ee _ -> m eo ee (\a _ _ _ _ -> eo a mempty)
  {-# INLINE lookAhead #-}

instance (Monoid state) => CharParsing (Parser state) where
  satisfy f = Parser $ \ _ ee co _ s d bs ->
    case UTF8.uncons $ Strict.drop (fromIntegral (columnByte d)) bs of
      Nothing        -> ee (Error s d NoReason mempty EpsilonError True)
      Just (c, xs)
        | not (f c)       -> ee (Error s (d <> delta c) NoReason mempty EpsilonError False)
        | Strict.null xs  -> let !ddc = d <> delta c
                             in join $ fillIt (co c mempty s ddc (if c == '\n' then mempty else bs))
                                              (co c mempty s)
                                              ddc
        | otherwise       -> co c mempty s (d <> delta c) bs
  {-# INLINE satisfy #-}

instance (Monoid state) => TokenParsing (Parser state)

instance (Monoid state) => DeltaParsing (Parser state) where
  line = Parser $ \eo _ _ _ _ _ bs -> eo bs mempty
  {-# INLINE line #-}
  position = Parser $ \eo _ _ _ _ d _ -> eo d mempty
  {-# INLINE position #-}
  rend = Parser $ \eo _ _ _ _ d bs -> eo (rendered d bs) mempty
  {-# INLINE rend #-}
  slicedWith f p = do
    m <- position
    a <- p
    r <- position
    f a <$> liftIt (sliceIt m r)
  {-# INLINE slicedWith #-}

data DeltaState s = DeltaState Delta s

instance HasDelta (DeltaState s) where
  delta (DeltaState d _) = d

getState :: (Monoid state) => Parser state state
getState = Parser $ \eo _ _ _ s _ _ -> eo s mempty
{-# INLINE getState #-}

modifyingState f (Parser m) = Parser $ \eo ee co ce s d bs ->
  m
    eo
    ee
    (\a es s' d' bs' -> co a es s d' bs')
    ce
    (f s)
    d
    bs
{-# INLINE modifyingState #-}

withDescription desc (Parser m) = Parser $ \eo ee ->
  m
    (\a e -> eo a (e { errorExpected = Set.singleton desc }))
    (\e   -> ee e { errorExpected    = Set.singleton desc })
{-# INLINE withDescription #-}

instance (Monoid state) => MarkParsing (DeltaState state) (Parser state) where
  mark = do
    delta <- position
    state <- getState
    return (DeltaState delta state)
  {-# INLINE mark #-}
  release (DeltaState d' s') = Parser $ \_ ee co _ _ d bs -> do
    mbs <- rewindIt d'
    case mbs of
      Just bs' -> co () mempty s' d' bs'
      Nothing
        | bytes d' == bytes (rewind d) + fromIntegral (Strict.length bs) -> if near d d'
            then co () mempty s' d' bs
            else co () mempty s' d' mempty
        | otherwise -> ee mempty

data Step state a
  = StepDone !Rope a
  | StepFail !Rope (Error state)
  | StepCont !Rope (Result state a) (Rope -> Step state a)

instance (Show state, Show a) => Show (Step state a) where
  showsPrec d (StepDone r a) = showParen (d > 10) $
    showString "StepDone " . showsPrec 11 r . showChar ' ' . showsPrec 11 a
  showsPrec d (StepFail r xs) = showParen (d > 10) $
    showString "StepFail " . showsPrec 11 r . showChar ' ' . showsPrec 11 xs
  showsPrec d (StepCont r fin _) = showParen (d > 10) $
    showString "StepCont " . showsPrec 11 r . showChar ' ' . showsPrec 11 fin . showString " ..."

instance Functor (Step state) where
  fmap f (StepDone r a)    = StepDone r (f a)
  fmap _ (StepFail r xs)   = StepFail r xs
  fmap f (StepCont r z k)  = StepCont r (fmap f z) (fmap f . k)

feed :: Reducer t Rope => t -> Step state r -> Step state r
feed t (StepDone r a)    = StepDone (snoc r t) a
feed t (StepFail r xs)   = StepFail (snoc r t) xs
feed t (StepCont r _ k)  = k (snoc r t)
{-# INLINE feed #-}

starve :: Step state a -> Result state a
starve (StepDone _ a)    = Right a
starve (StepFail _ xs)   = Left xs
starve (StepCont _ z _)  = z
{-# INLINE starve #-}

stepResult :: Rope -> Result state a -> Step state a
stepResult r (Right a) = StepDone r a
stepResult r (Left xs) = StepFail r xs
{-# INLINE stepResult #-}

data Stepping s a
  = EO a (Error s)
  | EE (Error s)
  | CO a (Set ParserDescription) s Delta ByteString
  | CE (Error s)

stepParser :: Parser state a -> Delta -> ByteString -> state -> Step state a
stepParser (Parser p) d0 bs0 s0 = go mempty $ p eo ee co ce s0 d0 bs0 where
  eo a e         = Pure (EO a e)
  ee e           = Pure (EE e)
  co a es s d bs = Pure (CO a es s d bs)
  ce errInf      = Pure (CE errInf)
  go r (Pure (EO a _))       = StepDone r a
  go r (Pure (EE e))         = StepFail r e
  go r (Pure (CO a _ _ _ _)) = StepDone r a
  go r (Pure (CE d))         = StepFail r d
  go r (It ma k)             = StepCont r (case ma of
                                  EO a _       -> Right a
                                  EE e         -> Left e
                                  CO a _ _ _ _ -> Right a
                                  CE e         -> Left e
                                ) (go <*> k)
{-# INLINE stepParser #-}

type Result s a = Either (Error s) a

-- | @parseFromFile p filePath@ runs a parser @p@ on the
-- input read from @filePath@ using 'ByteString.readFile'. All diagnostic messages
-- emitted over the course of the parse attempt are shown to the user on the console.
--
-- > main = do
-- >   result <- parseFromFile numbers "digits.txt"
-- >   case result of
-- >     Nothing -> return ()
-- >     Just a  -> print $ sum a
parseFromFile :: (Show state, Monoid state, MonadIO m) => Parser state a -> String -> state -> m (Maybe a)
parseFromFile p fn state = do
  result <- parseFromFileEx p fn state
  case result of
   Right a  -> return (Just a)
   Left  e -> do
     liftIO $ print e
     return Nothing

-- | @parseFromFileEx p filePath@ runs a parser @p@ on the
-- input read from @filePath@ using 'ByteString.readFile'. Returns all diagnostic messages
-- emitted over the course of the parse and the answer if the parse was successful.
--
-- > main = do
-- >   result <- parseFromFileEx (many number) "digits.txt"
-- >   case result of
-- >     Left  xs -> displayLn xs
-- >     Right a  -> print (sum a)
-- >

parseFromFileEx :: (Monoid state, MonadIO m) => Parser state a -> String -> state -> m (Result state a)
parseFromFileEx p fn state = do
  s <- liftIO $ Strict.readFile fn
  return $ parseByteString p (Directed (UTF8.fromString fn) 0 0 0 0) s state

-- | @parseByteString p delta i@ runs a parser @p@ on @i@.

parseByteString :: (Monoid state) => Parser state a -> Delta -> UTF8.ByteString -> state -> Result state a
parseByteString p d inp state = starve $ feed inp $ stepParser (release (DeltaState d state) *> p) mempty mempty state

parseString :: (Monoid state) => Parser state a -> Delta -> String -> state -> Result state a
parseString p d inp state = starve $ feed inp $ stepParser (release (DeltaState d state) *> p) mempty mempty state

parseTest :: (MonadIO m, Monoid state, Show a, Show state) => Parser state a -> String -> state -> m ()
parseTest p s state = case parseByteString p mempty (UTF8.fromString s) state of
  Left  xs -> liftIO $ print xs
  Right a  -> liftIO (print a)
