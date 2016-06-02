module Main where

import System.Console.Haskeline (runInputT, defaultSettings, getInputLine, outputStrLn)
import Mpl.Compiler             (compile, opts)

main :: IO ()
main = runInputT defaultSettings loop

loop = do
  line <- getInputLine "> "
  case line of
    Nothing     -> return ()
    Just "exit" -> return ()
    Just "quit" -> return ()
    Just a      -> do
      let (result, warnings, errors) = compile opts a
      mapM_ outputStrLn errors
      mapM_ outputStrLn warnings
      outputStrLn result
      outputStrLn ""
      loop
