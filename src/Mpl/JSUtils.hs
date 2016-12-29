module Mpl.JSUtils where

import Prelude hiding (lookup)
import Mpl.Utils
import Data.List as List
import Control.Monad.Trans.State.Strict (State, evalState, gets, modify')
import Data.Map.Strict as Map
import Language.JavaScript.Parser

type JSTranslator = State JSState JSExpression

data JSState =
  JSState
  { nativeIdentifiers :: Map.Map Text JSExpression
  , bindings          :: [(Text, Text)]
  , genSymCounter     :: Integer
  }

defaultJSState =
  JSState
  { nativeIdentifiers = Map.fromList [ ("+", jsCore "+") ]
  , bindings          = []
  , genSymCounter     = 0
  }

translateToJSExpression :: JSState -> JSTranslator -> JSExpression
translateToJSExpression = flip evalState

getNative ident = do
  idents <- gets nativeIdentifiers
  return $ Map.lookup ident idents

genSym :: State JSState Text
genSym = do
  currentIdent <- gets genSymCounter
  modify' (\s -> s { genSymCounter = genSymCounter s + 1 })
  return (stringToText $ "mplVar" ++ show currentIdent)

pushBinder :: Text -> Text -> State JSState ()
pushBinder mplSymbol jsSymbol = do
  modify' (\s -> s { bindings = (mplSymbol, jsSymbol) : bindings s })

popBinder :: State JSState ()
popBinder = do
  modify' (\s -> s { bindings = drop 1 (bindings s) })

findBinder :: Text -> State JSState (Maybe Text)
findBinder mplSymbol = do
  binds <- gets bindings
  return $ List.lookup mplSymbol binds

jsCore ident =
  JSCallExpressionSquare
    (JSIdentifier JSNoAnnot "JSCore")
    JSNoAnnot
    (JSStringLiteral JSNoAnnot $ show ident)
    JSNoAnnot
