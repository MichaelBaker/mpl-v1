module Main where

import System.Console.Haskeline (runInputT, defaultSettings, getInputLine, outputStrLn)
import Mpl.Compiler             (compile)

main :: IO ()
main = runInputT defaultSettings loop

loop = do
  line <- getInputLine "> "
  case line of
    Nothing     -> return ()
    Just "exit" -> return ()
    Just "quit" -> return ()
    Just a      -> outputStrLn (compile a) >> outputStrLn "" >> loop
