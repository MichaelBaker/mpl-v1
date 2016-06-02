module Mpl.ParserSpec where

import Test.Hspec

import Mpl.AST    (AST(..))
import Mpl.Parser (parse, aparen, asquare, acurly, aint, afloat, atext, asym)

test name string result = it name $ do
  let (parses, _) = parse string
  parses `shouldBe` [result]

spec :: Spec
spec = do
  describe "unit" $ do
    test "unit" "()" (aparen [])

  describe "integer" $ do
    test "a single digit"  "1"         (aint 1)
    test "a single number" "12"        (aint 12)
    test "long integer"    "987654321" (aint 987654321)

  describe "float" $ do
    test "leading zero" "0.0"                   (afloat 0.0)
    test "all digits"   "1234567890.0987654321" (afloat 1234567890.0987654321)

  describe "text" $ do
    test "empty text" "\"\""      (atext "")
    test "plain text" "\"hello\"" (atext "hello")

  describe "identifier" $ do
    test "single letter"       "a"                (asym "a")
    test "camel cased word"    "uUadkjADkdfsjljD" (asym "uUadkjADkdfsjljD")
    test "symbolic identifier" "<$>"              (asym "<$>")
    test "octothorp"           "#"                (asym "#")

  describe "list" $ do
    test "empty"                     "[]"                         (asquare [])
    test "an int"                    "[12]"                       (asquare [aint 12])
    test "two ints"                  "[12 13]"                    (asquare [aint 12, aint 13])
    test "leading inner whitespace"  "[ 12 13]"                   (asquare [aint 12, aint 13])
    test "leading outer whitespace"  " [12 13]"                   (asquare [aint 12, aint 13])
    test "trailing outer whitespace" "[12 13]  "                  (asquare [aint 12, aint 13])
    test "trailing inner whitespace" "[12 13 ]"                   (asquare [aint 12, aint 13])
    test "surrounding whitespace"    " [ 12 13]"                  (asquare [aint 12, aint 13])
    test "crazy"                     " [   1 53  23  8 12 13   ]" (asquare [aint 1,  aint 53, aint 23, aint 8, aint 12, aint 13])

  describe "map" $ do
    test "empty"         "{}"        (acurly [])
    test "string -> int" "{\"a\" 1}" (acurly [atext "a", aint 1])

  describe "function" $ do
    test "no arguments"       "(# [] 5)"             (aparen [asym "#", asquare [], aint 5])
    test "one argument"       "(# [a int] 5)"        (aparen [asym "#", asquare [asym "a", asym "int"], aint 5])
    test "multiple arguments" "(# [a int b unit] 5)" (aparen [asym "#", asquare [asym "a", asym "int", asym "b", asym "unit"], aint 5])

  describe "application" $ do
    test "simple application"        "(a 5)"             (aparen [asym "a", aint 5])
    test "application of a function" "((# [a int] a) 5)" (aparen [aparen [asym "#", asquare [asym "a", asym "int"], asym "a"], aint 5])
    test "forcing a thunk"           "((# [] 5))"        (aparen [aparen [asym "#", asquare [], aint 5]])
