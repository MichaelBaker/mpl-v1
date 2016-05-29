module Main where

import Mpl.Interpreter (interpret)
import System.Console.Haskeline (runInputT, defaultSettings, getInputLine, outputStrLn)

main :: IO ()
main = runInputT defaultSettings loop

loop = do
  line <- getInputLine "> "
  case line of
    Nothing     -> return ()
    Just "exit" -> return ()
    Just "quit" -> return ()
    Just a      -> outputStrLn (interpret a) >> outputStrLn "" >> loop
