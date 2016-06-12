module Mpl.Compiler where

import Mpl.AST             (AST)
import Mpl.Core            (Core(..))
import Mpl.ASTToCore       (astToCore)
import Mpl.Parser          (parse)
import Mpl.Interpreter     (eval)
import Mpl.TypeInterpreter (interpret)

import Text.Earley   (Report)
import Data.Text     (Text, pack)
import Data.List     (intercalate)

type Output = String

compile :: String -> Output
compile string = case run string of
  Left  s -> s
  Right s -> s

run :: String -> Either String Output
run string = do
  parsedAST <- Right $ parse (pack string)
  ast       <- handleParseFail parsedAST
  return $ prettyPrint $ eval $ interpret $ astToCore ast

handleParseFail :: ([AST], Report Text Text) -> Either String (AST)
handleParseFail (a:[], _)   = Right a
handleParseFail (a:rest, _) = Left $ "Error: The grammar is ambiguous and produced " ++ show (1 + length rest) ++ " parses.\n\n" ++ (intercalate "\n" $ map show (a:rest))
handleParseFail (_, r)      = Left $ show r

prettyPrint (CInt i) = show i
prettyPrint a = show a
