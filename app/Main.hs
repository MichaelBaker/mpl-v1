module Main where

import System.Console.Haskeline (runInputT, defaultSettings, getInputLine, outputStrLn)
import Mpl.Compiler             (Result(..), compile, opts)

main :: IO ()
main = runInputT defaultSettings loop

loop = do
  line <- getInputLine "> "
  case line of
    Nothing     -> return ()
    Just "exit" -> return ()
    Just "quit" -> return ()
    Just a      -> do
      let result = compile opts a
      mapM_ (outputStrLn . show) (errors result)
      mapM_ (outputStrLn . show) (warnings result)
      outputStrLn $ show result
      outputStrLn ""
      loop
