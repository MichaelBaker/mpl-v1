module Main where

import System.Console.Haskeline (runInputT, defaultSettings, getInputLine, outputStrLn, outputStr)
import Mpl.Utils                (stringToText, textToString, lazyTextToString, jsIR, envcata)
import Control.Monad.IO.Class   (liftIO)
import Repl.State               (State(..), StateLineItem(..), Mode(..), defaultState, toStateLineItem, showStateLine, strLineItem)
import Control.Monad            (replicateM_)
import Data.List                (transpose, isPrefixOf)
import Data.Ord                 (comparing)
import Text.Read                (readMaybe)

import qualified Text.Tabl                    as Table
import qualified System.Console.Terminal.Size as TermSize
import qualified System.Console.ANSI          as Term
import qualified Mpl.Untyped.BackendJS        as UntypedJS
import qualified Mpl.Untyped.Core             as UntypedCore
import qualified Mpl.Untyped.Parsing          as UntypedParser
import qualified V8

main :: IO ()
main = runInputT defaultSettings (loop defaultState)

data Action =
    Continue
  | NewState State
  | Exit

loop state = do
  outputStr Term.clearFromCursorToScreenEndCode
  outputState state
  input <- getInputLine "-> "
  outputStr Term.clearFromCursorToScreenEndCode
  case input of
    Nothing     -> return ()
    Just string -> do
      result <- handleInput string state
      case result of
        Exit              -> return ()
        Continue          -> loop state
        NewState newState -> loop newState

handleInput input state
  | ":quit" `isPrefixOf` input = return Exit
  | ":mode" `isPrefixOf` input = do
    let modeString = drop 5 input
    case readMaybe modeString of
      Just newMode -> return (NewState state { mode = newMode })
      Nothing -> do
        outputStrLn ("Invalid mode: " ++ modeString)
        return Continue
  | ":"     `isPrefixOf` input =
    let command = drop 1 input
    in outputStrLn ("Invalid command: '" ++ command ++ "'") >> return Continue
  | otherwise = do
      case mode state of
        Echo -> do
          outputStrLn input
        PrintAST -> do
          let textInput = stringToText input
          case snd $ UntypedParser.parseExpressionText textInput of
            Left e -> outputStrLn (show e)
            Right a -> outputStrLn (show a)
        PrintJS -> do
          let textInput = stringToText input
          case snd $ UntypedParser.parseExpressionText textInput of
            Left e ->
              outputStrLn (show e)
            Right a ->
              outputStrLn
              $ lazyTextToString
              $ jsIR
              $ UntypedJS.translateToJS
              $ envcata UntypedCore.syntaxToCore a
        EvalJS -> do
          let textInput = stringToText input
          case snd $ UntypedParser.parseExpressionText textInput of
            Left e ->
              outputStrLn (show e)
            Right a -> do
              let jsCode =
                    stringToText
                    $ lazyTextToString
                    $ jsIR
                    $ UntypedJS.translateToJS
                    $ envcata UntypedCore.syntaxToCore a
              result <- liftIO $ V8.withContext $ \context -> do
                V8.eval context jsCode
              outputStrLn (textToString result)
      return Continue

outputState state = do
  terminalWidth <- liftIO $ getTermWidth state
  let stateString = stateLine terminalWidth state
      stateLines  = length (lines stateString)
  replicateM_ stateLines (outputStrLn "")
  if stateLines > 1
    then outputStr $ Term.cursorUpLineCode (stateLines - 1)
    else return ()
  outputStr stateString
  outputStr $ Term.cursorUpLineCode stateLines

getTermWidth state = do
  terminalSize <- liftIO $ TermSize.size
  case terminalSize of
    Nothing     -> return (defaultTermWidth state)
    Just window -> return (TermSize.width window)

stateLine terminalWidth state = textToString $ renderTable cells
  where languageLine = toStateLineItem (Just "Language") (language state)
        modeLine     = toStateLineItem (Just "Mode") (mode state)
        stateItems   = [languageLine, modeLine]
        maxCellWidth = terminalWidth `div` length stateItems
        columns      = map (wrapText maxCellWidth . showStateLine 4) stateItems
        numRows      = maximum $ map length columns
        rows         = transpose $ map (padRows numRows) columns
        cells        = map (map stringToText) rows

padRows numRows column = column ++ replicate extraRows ""
  where extraRows = length column - numRows

wrapText maxWidth string
  | length string <= maxWidth = [string]
  | otherwise = take maxWidth string : wrapText maxWidth (drop maxWidth string)

renderTable cells =
  Table.tabl
    Table.EnvAscii
    Table.DecorNone
    Table.DecorNone
    []
    cells
