module Main where

import System.Console.Haskeline (runInputT, defaultSettings, getInputLine, outputStrLn)
import Mpl.Compiler             (compile)

main :: IO ()
main = putStrLn "Hello"
--main = runInputT defaultSettings loop

-- loop = do
--   line <- getInputLine "> "
--   case line of
--     Nothing     -> return ()
--     Just "exit" -> return ()
--     Just "quit" -> return ()
--     Just a      -> outputStrLn (show $ compile a) >> outputStrLn "" >> loop
