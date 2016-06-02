module Mpl.ParserSpec where

import Test.Hspec

import Mpl.AST    (AST(..), ASTType(..))
import Mpl.Parser (parse)

test name string result = it name $ do
  let (parses, _) = parse string
  parses `shouldBe` [result]

spec :: Spec
spec = do
  describe "unit" $ do
    test "unit" "()" (AUnit ())

  describe "integer" $ do
    test "a single digit"  "1"         (AInt () 1)
    test "a single number" "12"        (AInt () 12)
    test "long integer"    "987654321" (AInt () 987654321)

  describe "float" $ do
    test "leading zero" "0.0"                   (AFloat () 0.0)
    test "all digits"   "1234567890.0987654321" (AFloat () 1234567890.0987654321)

  describe "text" $ do
    test "empty text" "\"\""      (AText () "")
    test "plain text" "\"hello\"" (AText () "hello")

  describe "identifier" $ do
    test "single letter"       "a"                (AIdent () "a")
    test "camel cased word"    "uUadkjADkdfsjljD" (AIdent () "uUadkjADkdfsjljD")
    test "symbolic identifier" "<$>"              (AIdent () "<$>")

  describe "list" $ do
    test "empty"                     "[]"                         (AList () [])
    test "an int"                    "[12]"                       (AList () [AInt () 12])
    test "two ints"                  "[12 13]"                    (AList () [AInt () 12, AInt () 13])
    test "leading inner whitespace"  "[ 12 13]"                   (AList () [AInt () 12, AInt () 13])
    test "leading outer whitespace"  " [12 13]"                   (AList () [AInt () 12, AInt () 13])
    test "trailing outer whitespace" "[12 13]  "                  (AList () [AInt () 12, AInt () 13])
    test "trailing inner whitespace" "[12 13 ]"                   (AList () [AInt () 12, AInt () 13])
    test "surrounding whitespace"    " [ 12 13]"                  (AList () [AInt () 12, AInt () 13])
    test "crazy"                     " [   1,53, 23, 8 12,13  ,]" (AList () [AInt () 1,  AInt () 53, AInt () 23, AInt () 8, AInt () 12, AInt () 13])

  describe "map" $ do
    test "empty"         "{}"         (AMap () [])
    test "string -> int" "{\"a\": 1}" (AMap () [(AText () "a", AInt () 1)])

  describe "function" $ do
    test "no arguments"       "#([] 5)"             (AFunc () [] (AInt () 5))
    test "one argument"       "#([a int] 5)"        (AFunc () [("a", AIntTy)] (AInt () 5))
    test "multiple arguments" "#([a int b unit] 5)" (AFunc () [("a", AIntTy), ("b", AUnitTy)] (AInt () 5))

  describe "application" $ do
    test "simple application"        "(a 5)"            (AApp () (AIdent () "a") [(AInt () 5)])
    test "application of a function" "(#([a int] a) 5)" (AApp () (AFunc () [("a", AIntTy)] (AIdent () "a")) [(AInt () 5)])
    test "forcing a thunk"           "(#([] 5))"        (AApp () (AFunc () [] (AInt () 5)) [])
