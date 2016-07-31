{-# LANGUAGE RecursiveDo #-}

module Mpl.Dyn.Parser where

import Mpl.Dyn.AST          (AST(..))
-- import Control.Applicative  ((<|>), many, some, optional)
import Text.Parser.Token    (integer)
import Text.Trifecta.Result (Result())
import Text.Trifecta.Parser (parseFromFileEx)

data ParseType = Exp | Prog

parseFile :: ParseType -> String -> IO (Result AST)
parseFile Exp  filepath = parseFromFileEx expression filepath
parseFile Prog filepath = parseFromFileEx expression filepath

expression = AInt <$> integer

-- grammar :: ParseType -> Grammar r (Prod r Text Char (AST))
-- grammar parseType = mdo
--   let exp           = int
--       symChars      = ['<', '>', '?', '~', '!', '@', '#', '$', '%', '^', '&', '*', '-', '_', '+', '|', '\\', '/', '.']
--       naturalDigits = ['1', '2', '3', '4', '5', '6', '7', '8', '9']
--       floating    a = spaceAfter a <|> a
--       spaceAfter  a = a <* some (satisfy whitespace)
--       whitespace    = isSpace
--       separated cons es Nothing  = cons es
--       separated cons es (Just l) = cons (es ++ [l])
--       skipWhitespace = many (satisfy whitespace)
-- 
--   prog <- rule $ (separated AProg)
--      <$> many (spaceAfter def)
--      <*> optional def
--      <?> "program"
-- 
--   int <- rule $ (\firstDigit rest -> AInt $ forceRead (signed decimal) $ pack $ firstDigit:rest)
--     <$> satisfy (`elem` naturalDigits)
--     <*> many (satisfy isDigit)
--     <?> "integer"
-- 
--   def <- rule $ ADef <$> spaceAfter sym <* spaceAfter (token '=') <*> exp <?> "definition"
-- 
--   sym <- rule $ (\a as -> ASym $ pack (a:as))
--     <$> satisfy (\c -> any ($ c) [isAsciiLower, (`elem` symChars)])
--     <*> many (satisfy (\c -> any ($ c) [isDigit, isAsciiLower, isAsciiUpper, (`elem` symChars)]))
--     <?> "symbol"
-- 
--   return $ case parseType of
--     Exp  -> skipWhitespace *> floating exp
--     Prog -> skipWhitespace *> floating prog
-- 
-- forceRead reader a = case reader a of
--   Left e  -> error e
--   Right b -> if T.null (snd b)
--     then fst b
--     else error ("Unconsumed input: " ++ (show $ snd b))
-- 
