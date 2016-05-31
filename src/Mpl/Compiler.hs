module Mpl.Compiler where

import Mpl.AST            (AST)
import Mpl.Desugar        (desugar)
import Mpl.TypeAnnotation (annotate)
import Mpl.Parser         (parse)
import Mpl.Interpreter    (interpret)

import Text.Earley   (Report)
import Data.Text     (Text, pack)
import Data.List     (intercalate)

import Prelude hiding (curry)

compile :: String -> String
compile string = case run string of
  Left  s -> s
  Right s -> s

run :: String -> Either String String
run string = do
  parsedAST <- Right $ parse (pack string)
  ast       <- handleParseFail parsedAST
  result    <- Right $ interpret $ annotate $ desugar ast
  return $ show result

handleParseFail :: ([AST ()], Report Text Text) -> Either String (AST ())
handleParseFail (a:[], _)   = Right a
handleParseFail (a:rest, _) = Left $ "Error: The grammar is ambiguous and produced " ++ show (1 + length rest) ++ " parses.\n\n" ++ (intercalate "\n" $ map show (a:rest))
handleParseFail (_, r)      = Left $ show r
