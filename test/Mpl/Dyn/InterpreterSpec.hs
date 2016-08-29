module Mpl.Dyn.InterpreterSpec where

import Test.Hspec
import Helper.Test
import Mpl.Dyn.Parser       (ParseType(Exp), parseFile, parseString)
import Mpl.Dyn.Desugar      (desugar)
import Mpl.Dyn.Interpreter  (interpret)

testFile   = makeTestFile "test/TestCases/Dyn/Interpreter/" parseFile (interpret . desugar)
testString = makeTestString parseString (interpret . desugar)

-- TODO
-- Add a test for closures
-- Calling something that isn't a function

spec :: Spec
spec = do
  describe "integer" $ do
    testString "simple integer" "234" Exp "234"

  describe "real" $ do
    testString "simple real" "234.123" Exp "234.123"

  describe "symbol" $ do
    testString "undefined symbol" "a" Exp "\"<Undefined symbol 'a'>\""

  describe "list" $ do
    testString "list of ints" "[1,2,3]" Exp "[1, 2, 3]"

  describe "record" $ do
    testString "two field record" "{ 1: \"a\", wat: \"yep\" }" Exp "{wat: \"yep\", 1: \"a\"}"

  describe "utf16" $ do
    testString "simple utf16 string" "\"hello\"" Exp "\"hello\""

  describe "let" $ do
    testString "simple let" "let a = 5 in a" Exp "5"
    testFile   "two bindings" "let-00.mpldyn" Exp "6"

  describe "lambda" $ do
    testString "simple thunk" "(# 5)" Exp "(# 5)"
    testString "simple lambda" "(# a = a)" Exp "(# a = a)"

  describe "application" $ do
    testString "simple application" "let f a = a in f 5" Exp "5"

  describe "lensApplication" $ do
    testString "lens application" "let a = {b: \"hello\"} in a.b" Exp "\"hello\""
