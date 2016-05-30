module Mpl.ParserSpec where

import Test.Hspec

import Mpl.AST    (AST(..))
import Mpl.Parser (parse)

parses name string result = it name $ do
  let (parses, _) = parse string
  parses `shouldBe` [result]

spec :: Spec
spec = do
  describe "grammar" $ do
    describe "integer" $ do
      parses "a single digit"  "1"         (AInt () 1)
      parses "a single number" "12"        (AInt () 12)
      parses "long integer"    "987654321" (AInt () 987654321)

    describe "float" $ do
      parses "leading zero" "0.0"                   (AFloat () 0.0)
      parses "all digits"   "1234567890.0987654321" (AFloat () 1234567890.0987654321)

    describe "text" $ do
      parses "empty text" "\"\""      (AText () "")
      parses "plain text" "\"hello\"" (AText () "hello")

    describe "identifier" $ do
      parses "single letter"       "a"                (AIdent () "a")
      parses "camel cased word"    "uUadkjADkdfsjljD" (AIdent () "uUadkjADkdfsjljD")
      parses "symbolic identifier" "<$>"              (AIdent () "<$>")

    describe "list" $ do
      parses "empty"                     "[]"                         (AList () [])
      parses "an int"                    "[12]"                       (AList () [AInt () 12])
      parses "two ints"                  "[12 13]"                    (AList () [AInt () 12, AInt () 13])
      parses "leading inner whitespace"  "[ 12 13]"                   (AList () [AInt () 12, AInt () 13])
      parses "leading outer whitespace"  " [12 13]"                   (AList () [AInt () 12, AInt () 13])
      parses "trailing outer whitespace" "[12 13]  "                  (AList () [AInt () 12, AInt () 13])
      parses "trailing inner whitespace" "[12 13 ]"                   (AList () [AInt () 12, AInt () 13])
      parses "surrounding whitespace"    " [ 12 13]"                  (AList () [AInt () 12, AInt () 13])
      parses "crazy"                     " [   1,53, 23, 8 12,13  ,]" (AList () [AInt () 1,  AInt () 53, AInt () 23, AInt () 8, AInt () 12, AInt () 13])

    describe "map" $ do
      parses "empty"         "{}"         (AMap () [])
      parses "string -> int" "{\"a\": 1}" (AMap () [(AText () "a", AInt () 1)])

    describe "function" $ do
      parses "no arguments" "#([] 5)" (AFunc () (AList () []) (AInt () 5))

    describe "application" $ do
      parses "simple application" "(a 5)" (AApp () (AIdent () "a") [(AInt () 5)])
